#!/usr/bin/env bb

(ns repl
  (:require
   [babashka.fs :as fs]
   [babashka.process :as bp]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]))

(def cli-options
  ;; An option with a required argument
  [["--no-cider" "Don't include cider nrepl middlewares"
    :default false]
   ["-h" "--help"]])

(:options (parse-opts *command-line-args* cli-options))

(def deps-edn-locations
  (let [home (System/getProperty "user.home")]
    [(str home "/.clojure/deps.edn") "./deps.edn"]))

(defn get-aliases []
  (into (sorted-set)
        cat
        (mapv (fn [p]
                (when (fs/exists? p)
                  (-> p
                      slurp
                      edn/read-string
                      :aliases
                      keys)))
              deps-edn-locations)))

(def aliases (get-aliases))

(defn gum-choose
  [options {:keys [selected]}]
  (let [options (str/join " " options)
        selected (if (seq selected) (str "--selected " (str/join "," selected)) "")
        cmd (format "gum choose %s %s %s" "--no-limit" options selected)]
    (-> (bp/shell {:out :string} cmd)
        :out
        str/trim
        str/split-lines)))

(defn gum-filter [options]
  (let [options (str "\"" (str/join "\n" options) "\"")
        echo-options (format "echo -e %s" options)
        gum-cmd "gum filter --no-limit"]
    (->
     (bp/shell {:out :string} echo-options)
     (bp/shell {:out :string} gum-cmd)
     :out
     str/trim
     str/split-lines)))

(defn run-clj-command [aliases]
  (let [aliases-opt (if (seq aliases)
                      (str "-M"
                           (when true ":cider/nrepl")
                           (str/join "" aliases))
                      "")
        cider-nrepl-mw (if true
                         "-Sdeps '{:deps {nrepl/nrepl {:mvn/version \"0.9.0\"} refactor-nrepl/refactor-nrepl {:mvn/version \"3.1.0\"} cider/cider-nrepl {:mvn/version \"0.27.4\"}} :aliases {:cider/nrepl {:main-opts [\"-m\" \"nrepl.cmdline\" \"--middleware\" \"[refactor-nrepl.middleware/wrap-refactor,cider.nrepl/cider-middleware]\"]}}}'"
                         "")]
    (format "clojure %s %s" cider-nrepl-mw aliases-opt)))

(defn save-choices! [choice]
  (fs/write-lines ".repl-alias" choice))

(defn read-saved-choices! []
  (when (fs/exists? ".repl-alias")
    (let [saved (fs/read-all-lines ".repl-alias")]
      (filter #(contains? aliases (read-string %)) saved))))

(defn main []
  (println "Choose your aliases")
  (let [saved-choices (read-saved-choices!)
        choice (gum-choose aliases {:selected saved-choices})
        cmd (run-clj-command choice)]
    (save-choices! choice)
    (println cmd)
    (bp/shell cmd)))

(main)

(comment)
