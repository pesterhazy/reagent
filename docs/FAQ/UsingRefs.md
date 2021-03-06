### Question

When using Reagent, how do I use React's `refs`?

### Answer

Credit: this entry is entirely based on Paulus Esterhazy's [Reagent Mysteries series](https://presumably.de/reagent-mysteries-part-3-manipulating-the-dom.html)

We'll start with a code fragment, because it is worth a 1000 words:

```cljs
(defn video-ui []
  (let [!video (clojure.core/atom nil)]    ;; stores the
	(fn [{:keys [src]}]
	  [:div
	   [:div
		[:video {:src src
				 :style {:width 400}
				 :ref (fn [el]
						(reset! !video el))}]]
	   [:div
		[:button {:on-click (fn []
							  (when-let [video @!video] ;; not nil?
								(if (.-paused video)
								  (.play video)
								  (.pause video))))}
		 "Toogle"]]])))
```

Notes:
   1. That's a Form-2 component. That allows us to retain state outside of the renderer `fn`.
   2. We capture state in `!video`. The state we capture is a reference to a video component.
   2. `!video` is a `clojure.core/atom` and not a `reaagent.core/atom`
   4. On the `:video` component there's a `:ref` callback function which establishes the state in `!video`
   5. Thereafter, `@!video` is used with the `:button's` `:on-click` to manipulate the `video`
   5. For full notes [read Paulus' blog post](https://presumably.de/reagent-mysteries-part-3-manipulating-the-dom.html)
   6. For more background on callback refs, see [React's documentation](https://facebook.github.io/react/docs/more-about-refs.html)

***

Up:  [FAQ Index](../README.md)&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
