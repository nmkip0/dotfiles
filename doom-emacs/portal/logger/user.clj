(require '[clojure.datafy :as d])
(require '[clojure.instant :as i])
(require '[portal.api :as p])
(require '[taoensso.timbre :as log])

(defn log->portal [{:keys [level ?err msg_ timestamp_ ?ns-str ?file context ?line]}]
  (merge
   (when ?err
     {:error (d/datafy ?err)})
   (when-let [ts (force timestamp_)]
     {:time (i/read-instant-date ts)})
   {:level   level
    :ns      (symbol (or ?ns-str ?file "?"))
    :line    (or ?line 1)
    :column  1
    :result  (force msg_)
    :runtime :clj}
   context))

(defonce logs (atom '()))

(defn log
  "Accumulate a rolling log of 100 entries."
  [log]
  (swap! logs
         (fn [logs]
           (take 100 (conj logs (log->portal log))))))

(defn setup []
  (reset! logs '())
  (log/merge-config!
   {:appenders
    {:memory {:enabled? true :fn #'log}}}))

(defn open []
  (p/open {:window-title "Logs Viewer" :value logs :port 4999}))

(setup)
(def user/portal-logger (open))

(comment
  @logs
  (setup)
  (open)
  log/*config*

  log/*context*
  (log/info  "my info log")
  (log/error (ex-info "my exception" {:hello :world}) "my error log")

  (log/with-context+
    {:runtime :cljs}
    (log/info "my cljs log")))
