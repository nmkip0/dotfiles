(def defaults
  {vector? :portal.viewer/tree})

(defn get-viewer [value]
  (some (fn [[predicate viewer]]
          (when (predicate value)
            viewer))
        defaults))

(defn submit**
  [submit-fn opts value]
  (if-let [viewer (get-viewer value)]
    (submit-fn
     opts
     (with-meta value
       {:portal.viewer/default viewer}))
    (submit-fn
     opts
     value)))
