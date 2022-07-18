(require '[clojure.datafy :as d])
(require '[portal.api :as portal.api])
(require '[portal.client.jvm :as portal.jvm])
(require '[portal.client.node :as portal.node])

(defn js->clj+ [obj]
  (if (goog.isObject obj)
    (-> (fn [result key]
          (let [v (goog.object/get obj key)]
            (if (= \"function \" (goog/typeOf v))
              result
              (assoc result (keyword key) (obj->clj v)))))
        (reduce {} (.getKeys goog/object obj)))
    obj))

(def submit (comp (partial portal.node/submit {:port 5678})))

(add-tap #'submit)

(tap> :portal-node)

