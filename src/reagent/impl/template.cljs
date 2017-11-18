(ns reagent.impl.template
  (:require [react :as react]
            [clojure.string :as string]
            [clojure.walk :refer [prewalk]]
            [reagent.impl.util :as util :refer [is-client]]
            [reagent.impl.component :as comp]
            [reagent.impl.batching :as batch]
            [reagent.ratom :as ratom]
            [reagent.debug :refer-macros [dbg prn println log dev?
                                          warn warn-unless]]
            [goog.object :as gobj]))

;; From Weavejester's Hiccup, via pump:
(def ^{:doc "Regular expression that parses a CSS-style id and class
             from a tag name."}
  re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(deftype NativeWrapper [])


;;; Common utilities

(defn ^boolean named? [x]
  (or (keyword? x)
      (symbol? x)))

(defn ^boolean hiccup-tag? [x]
  (or (named? x)
      (string? x)))

(defn ^boolean valid-tag? [x]
  (or (hiccup-tag? x)
      (ifn? x)
      (instance? NativeWrapper x)))


;;; Props conversion

(def prop-name-cache #js{:class "className"
                         :for "htmlFor"
                         :charset "charSet"})

(defn cache-get [o k]
  (when ^boolean (.hasOwnProperty o k)
    (gobj/get o k)))

(defn cached-prop-name [k]
  (if (named? k)
    (if-some [k' (cache-get prop-name-cache (name k))]
      k'
      (let [v (util/dash-to-camel k)]
        (gobj/set prop-name-cache (name k))
        v))
    k))

(defn ^boolean js-val? [x]
  (not (identical? "object" (goog/typeOf x))))

(declare convert-prop-value)

(defn kv-conv [o k v]
  (doto o
    (gobj/set (cached-prop-name k) (convert-prop-value v))))

(defn convert-prop-value [x]
  (cond (js-val? x) x
        (named? x) (name x)
        (map? x) (reduce-kv kv-conv #js{} x)
        (coll? x) (clj->js x)
        (ifn? x) (fn [& args]
                   (apply x args))
        :else (clj->js x)))

(defn oset [o k v]
  (doto (if (nil? o) #js {} o)
    (gobj/set k v)))

(defn oget [o k]
  (if (nil? o) nil (gobj/get o k)))

(defn set-id-class [p id-class]
  (let [id (.-id id-class)
        p (if (and (some? id)
                   (nil? (oget p "id")))
            (oset p "id" id)
            p)]
    (if-some [class (.-className id-class)]
      (let [old (oget p "className")]
        (oset p "className" (if (nil? old)
                              class
                              (str class " " old))))
      p)))

(defn stringify-class [{:keys [class] :as props}]
  (if (coll? class)
    (->> class
         (filter identity)
         (string/join " ")
         (assoc props :class))
    props))

(defn convert-props [props id-class]
  (-> props
      stringify-class
      convert-prop-value
      (set-id-class id-class)))

;;; Specialization for input components

;; This gets set from reagent.dom
(defonce find-dom-node nil)

;; <input type="??" >
;; The properites 'selectionStart' and 'selectionEnd' only exist on some inputs
;; See: https://html.spec.whatwg.org/multipage/forms.html#do-not-apply
(def these-inputs-have-selection-api #{"text" "textarea" "password" "search"
                                       "tel" "url"})

(defn ^boolean has-selection-api?
  [input-type]
  (contains? these-inputs-have-selection-api input-type))

(declare input-component-set-value)

(defn input-node-set-value
  [node rendered-value dom-value component {:keys [on-write]}]
  (if-not (and (identical? node (.-activeElement js/document))
            (has-selection-api? (.-type node))
            (string? rendered-value)
            (string? dom-value))
    ;; just set the value, no need to worry about a cursor
    (do
      (set! (.-cljsDOMValue component) rendered-value)
      (set! (.-value node) rendered-value)
      (when (fn? on-write)
        (on-write rendered-value)))

    ;; Setting "value" (below) moves the cursor position to the
    ;; end which gives the user a jarring experience.
    ;;
    ;; But repositioning the cursor within the text, turns out to
    ;; be quite a challenge because changes in the text can be
    ;; triggered by various events like:
    ;; - a validation function rejecting a user inputted char
    ;; - the user enters a lower case char, but is transformed to
    ;;   upper.
    ;; - the user selects multiple chars and deletes text
    ;; - the user pastes in multiple chars, and some of them are
    ;;   rejected by a validator.
    ;; - the user selects multiple chars and then types in a
    ;;   single new char to repalce them all.
    ;; Coming up with a sane cursor repositioning strategy hasn't
    ;; been easy ALTHOUGH in the end, it kinda fell out nicely,
    ;; and it appears to sanely handle all the cases we could
    ;; think of.
    ;; So this is just a warning. The code below is simple
    ;; enough, but if you are tempted to change it, be aware of
    ;; all the scenarios you have handle.
    (let [node-value (.-value node)]
      (if (not= node-value dom-value)
        ;; IE has not notified us of the change yet, so check again later
        (batch/do-after-render #(input-component-set-value component))
        (let [existing-offset-from-end (- (count node-value)
                                         (.-selectionStart node))
              new-cursor-offset        (- (count rendered-value)
                                         existing-offset-from-end)]
          (set! (.-cljsDOMValue component) rendered-value)
          (set! (.-value node) rendered-value)
          (when (fn? on-write)
            (on-write rendered-value))
          (set! (.-selectionStart node) new-cursor-offset)
          (set! (.-selectionEnd node) new-cursor-offset))))))

(defn input-component-set-value [this]
  (when (.-cljsInputLive this)
    (set! (.-cljsInputDirty this) false)
    (let [rendered-value (.-cljsRenderedValue this)
          dom-value (.-cljsDOMValue this)
          node (find-dom-node this) ;; Default to the root node within this component
          synthetic-on-update (.-cljsSyntheticOnUpdate this)]
      (when (not= rendered-value dom-value)
        (if (fn? synthetic-on-update)
          (synthetic-on-update input-node-set-value node rendered-value dom-value this)
          (input-node-set-value node rendered-value dom-value this {}))))))

(defn input-handle-change [this on-change e]
  (set! (.-cljsDOMValue this) (-> e .-target .-value))
  ;; Make sure the input is re-rendered, in case on-change
  ;; wants to keep the value unchanged
  (when-not (.-cljsInputDirty this)
    (set! (.-cljsInputDirty this) true)
    (batch/do-after-render #(input-component-set-value this)))
  (on-change e))

(defn input-render-setup
  ([this jsprops {:keys [synthetic-on-update synthetic-on-change]}]
   ;; Don't rely on React for updating "controlled inputs", since it
   ;; doesn't play well with async rendering (misses keystrokes).
   (when (and (some? jsprops)
           (.hasOwnProperty jsprops "onChange")
           (.hasOwnProperty jsprops "value"))
     (assert find-dom-node
       "reagent.dom needs to be loaded for controlled input to work")
     (when synthetic-on-update
       ;; Pass along any synthetic input setter given
       (set! (.-cljsSyntheticOnUpdate this) synthetic-on-update))
     (let [v (.-value jsprops)
           value (if (nil? v) "" v)
           on-change (.-onChange jsprops)
           on-change (if synthetic-on-change
                       (partial synthetic-on-change on-change)
                       on-change)]
       (when-not (.-cljsInputLive this)
         ;; set initial value
         (set! (.-cljsInputLive this) true)
         (set! (.-cljsDOMValue this) value))
       (set! (.-cljsRenderedValue this) value)
       (js-delete jsprops "value")
       (set! (.-defaultValue jsprops) value)
       (set! (.-onChange jsprops) #(input-handle-change this on-change %)))))
  ([this jsprops]
   (input-render-setup this jsprops {})))

(defn input-unmount [this]
  (set! (.-cljsInputLive this) nil))

(defn ^boolean input-component? [x]
  (case x
    ("input" "textarea") true
    false))

(def reagent-input-class nil)

(def reagent-synthetic-input-class nil)

(declare make-element)

(def input-spec
  {:display-name "ReagentInput"
   :component-did-update input-component-set-value
   :component-will-unmount input-unmount
   :reagent-render
   (fn [argv comp jsprops first-child]
     (let [this comp/*current-component*]
       (input-render-setup this jsprops)
       (make-element argv comp jsprops first-child)))})

(def synthetic-input-spec
  ;; Same as `input-spec` except it takes another argument for `input-setter`
  {:display-name "ReagentSyntheticInput"
   :component-did-update input-component-set-value
   :component-will-unmount input-unmount
   :reagent-render
   (fn [on-update on-change argv comp jsprops first-child]
     (let [this comp/*current-component*]
       (input-render-setup this jsprops {:synthetic-on-update on-update
                                         :synthetic-on-change on-change})
       (make-element argv comp jsprops first-child)))})


(defn reagent-input
  []
  (when (nil? reagent-input-class)
    (set! reagent-input-class (comp/create-class input-spec)))
  reagent-input-class)

(defn reagent-synthetic-input
  []
  (when (nil? reagent-synthetic-input-class)
    (set! reagent-synthetic-input-class (comp/create-class synthetic-input-spec)))
  reagent-synthetic-input-class)


;;; Conversion from Hiccup forms

(defn parse-tag [hiccup-tag]
  (let [[tag id class] (->> hiccup-tag name (re-matches re-tag) next)
        class (when-not (nil? class)
                (string/replace class #"\." " "))]
    (assert tag (str "Invalid tag: '" hiccup-tag "'"
                     (comp/comp-name)))
    #js{:name tag
        :id id
        :className class}))

(defn try-get-key [x]
  ;; try catch to avoid clojurescript peculiarity with
  ;; sorted-maps with keys that are numbers
  (try (get x :key)
       (catch :default e)))

(defn get-key [x]
  (when (map? x)
    (try-get-key x)))

(defn key-from-vec [v]
  (if-some [k (-> (meta v) get-key)]
    k
    (-> v (nth 1 nil) get-key)))

(defn reag-element [tag v]
  (let [c (comp/as-class tag)
        jsprops #js {:argv v}]
    (when-some [key (key-from-vec v)]
      (set! (.-key jsprops) key))
    (react/createElement c jsprops)))

(defn adapt-react-class
  ([c {:keys [synthetic-input]}]
   (let [on-update (:on-update synthetic-input)
         on-change (:on-change synthetic-input)]
     (when synthetic-input
       (assert (fn? on-update))
       (assert (fn? on-change)))
     (let [wrapped (->NativeWrapper)
           _ (set! (.-name wrapped) c)
           _ (set! (.-id wrapped) nil)
           _ (set! (.-class wrapped) nil)
           _ (if synthetic-input
               (set! (.-syntheticInput wrapped) true))
           _ (if synthetic-input
               (set! (.-syntheticOnChange wrapped) on-change))
           ;; This is a synthetic input component, i.e. it has a complex
           ;; nesting of elements such that the root node is not necessarily
           ;; the <input> tag we need to control, and/or it needs to execute
           ;; custom code when updated values are written so we provide an affordance
           ;; to configure a setter fn that can choose a different DOM node
           ;; than the root node if it wants, and can supply a function hooked
           ;; to value updates so it can maintain its own component state as needed.
           _ (if synthetic-input
               (set! (.-syntheticOnUpdate wrapped) on-update))]
       wrapped)))
  ([c]
   (adapt-react-class c {})))

(def tag-name-cache #js{})

(defn cached-parse [x]
  (if-some [s (cache-get tag-name-cache x)]
    s
    (let [v  (parse-tag x)]
      (gobj/set tag-name-cache x v)
      v)))

(declare as-element)

(defn native-element [parsed argv first]
  (let [comp (.-name parsed)
        synthetic-input (.-syntheticInput parsed)]
    (let [props (nth argv first nil)
          hasprops (or (nil? props) (map? props))
          jsprops (convert-props (if hasprops props) parsed)
          first-child (+ first (if hasprops 1 0))]
      (if (or synthetic-input (input-component? comp))
        (-> (if synthetic-input
              ;; If we are dealing with a synthetic input, use the synthetic-input-spec form:
              [(reagent-synthetic-input)
               (.-syntheticOnUpdate parsed)
               (.-syntheticOnChange parsed)
               argv
               comp
               jsprops
               first-child]
              ;; Else use the regular input-spec form:
              [(reagent-input) argv comp jsprops first-child])
            (with-meta (meta argv))
            as-element)
        (let [key (-> (meta argv) get-key)
              p (if (nil? key)
                  jsprops
                  (oset jsprops "key" key))]
          (make-element argv comp p first-child))))))

(defn str-coll [coll]
  (if (dev?)
    (str (prewalk (fn [x]
                    (if (fn? x)
                      (let [n (util/fun-name x)]
                        (case n "" x (symbol n)))
                      x)) coll))
    (str coll)))

(defn hiccup-err [v & msg]
  (str (apply str msg) ": " (str-coll v) "\n" (comp/comp-name)))

(defn vec-to-elem [v]
  (assert (pos? (count v)) (hiccup-err v "Hiccup form should not be empty"))
  (let [tag (nth v 0 nil)]
    (assert (valid-tag? tag) (hiccup-err v "Invalid Hiccup form"))
    (cond
      (hiccup-tag? tag)
      (let [n (name tag)
            pos (.indexOf n ">")]
        (case pos
          -1 (native-element (cached-parse n) v 1)
          0 (let [comp (nth v 1 nil)]
              ;; Support [:> comp ...]
              (assert (= ">" n) (hiccup-err v "Invalid Hiccup tag"))
              (assert (or (string? comp) (fn? comp))
                      (hiccup-err v "Expected React component in"))
              (native-element #js{:name comp} v 2))
          ;; Support extended hiccup syntax, i.e :div.bar>a.foo
          (recur [(subs n 0 pos)
                  (assoc v 0 (subs n (inc pos)))])))

      (instance? NativeWrapper tag)
      (native-element tag v 1)

      :else (reag-element tag v))))

(declare expand-seq)
(declare expand-seq-check)

(defn as-element [x]
  (cond (js-val? x) x
        (vector? x) (vec-to-elem x)
        (seq? x) (if (dev?)
                   (expand-seq-check x)
                   (expand-seq x))
        (named? x) (name x)
        (satisfies? IPrintWithWriter x) (pr-str x)
        :else x))

(set! comp/as-element as-element)

(defn expand-seq [s]
  (let [a (into-array s)]
    (dotimes [i (alength a)]
      (aset a i (as-element (aget a i))))
    a))

(defn expand-seq-dev [s o]
  (let [a (into-array s)]
    (dotimes [i (alength a)]
      (let [val (aget a i)]
        (when (and (vector? val)
                   (nil? (key-from-vec val)))
          (set! (.-no-key o) true))
        (aset a i (as-element val))))
    a))

(defn expand-seq-check [x]
  (let [ctx #js{}
        [res derefed] (ratom/check-derefs #(expand-seq-dev x ctx))]
    (when derefed
      (warn (hiccup-err x "Reactive deref not supported in lazy seq, "
                        "it should be wrapped in doall")))
    (when (.-no-key ctx)
      (warn (hiccup-err x "Every element in a seq should have a unique :key")))
    res))

;; From https://github.com/babel/babel/commit/1d0e68f5a19d721fe8799b1ea331041d8bf9120e
;; (def react-element-type (or (and (exists? js/Symbol)
;;                                  ($ js/Symbol :for)
;;                                  ($ js/Symbol for "react.element"))
;;                             60103))

;; (defn make-element-fast [argv comp jsprops first-child]
;;   (let [key (some-> jsprops ($ :key))
;;         ref (some-> jsprops ($ :ref))
;;         props (if (nil? jsprops) (js-obj) jsprops)]
;;     ($! props :children
;;         (case (- (count argv) first-child)
;;           0 nil
;;           1 (as-element (nth argv first-child))
;;           (reduce-kv (fn [a k v]
;;                        (when (>= k first-child)
;;                          (.push a (as-element v)))
;;                        a)
;;                      #js[] argv)))
;;     (js-obj "key" key
;;             "ref" ref
;;             "props" props
;;             "$$typeof" react-element-type
;;             "type" comp
;;             ;; "_store" (js-obj)
;;             )))

(defn make-element [argv comp jsprops first-child]
  (case (- (count argv) first-child)
    ;; Optimize cases of zero or one child
    0 (react/createElement comp jsprops)

    1 (react/createElement comp jsprops
          (as-element (nth argv first-child nil)))

    (.apply react/createElement nil
            (reduce-kv (fn [a k v]
                         (when (>= k first-child)
                           (.push a (as-element v)))
                         a)
                       #js[comp jsprops] argv))))
