(require '[clojure.datafy :as d])
(require '[portal.api :as portal.api])

(load-file "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/test-runner/user.cljc")

(def user/portal (portal.api/open))

(def submit (comp portal.api/submit d/datafy))

(add-tap #'submit)

(tap> :portal-connected)

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
  (catch #?(:clj Throwable :cljs :default) _))
