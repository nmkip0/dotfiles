(require '[clojure.test  :as t])

(defn run-ns-tests [ns]
  (let [ns (if (symbol? ns)
             ns
             (symbol ns))
        report (atom [])]
    (with-redefs [t/report (fn [r]
                             (swap! report conj r))]
      (t/run-tests ns)
      (tap> report))))
