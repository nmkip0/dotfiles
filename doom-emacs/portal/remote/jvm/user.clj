(require '[clojure.datafy :as d])
(require '[portal.api :as portal.api])
(require '[portal.client.jvm :as portal.jvm])

(def config (read-string (slurp "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/config.edn")))

(load-file "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/utils.cljc")

(defn port
  [config]
  (or (:port config)
      5678))

(defn submit*
  [value]
  (if-let [viewer (get-viewer value)]
    (portal.jvm/submit
     {:port (port config)}
     (with-meta value
       {:portal.viewer/default viewer}))
    (portal.jvm/submit
     {:port (port config)}
     value)))

(def submit (comp #_(partial portal.jvm/submit {:port (port config)})
             (partial submit*)
                  d/datafy))

(add-tap #'submit)

(try
  (let [r!   (requiring-resolve 'portal.runtime/register!)
        html (fn [url]
               (with-meta
                 [:div
                  {:style {:background :white}}
                  [:portal.viewer/html [:iframe {:src url}]]]
                 {:portal.viewer/default :portal.viewer/hiccup}))]
         ;; install extra functions:
    (run! (fn [[k f]] (r! f {:name k}))
          {'dev/->file   (requiring-resolve 'clojure.java.io/file)
           'dev/->html   html
           'dev/->map    (partial into {})
           'dev/->set    (partial into #{})
           'dev/->vector (partial into [])}))
  (catch Throwable _))
