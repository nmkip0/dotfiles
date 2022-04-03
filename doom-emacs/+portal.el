;;; package --- Summary
;;; Commentary:

;;; Code:

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api)
    (portal.api/tap)
    (do (def user/portal (portal.api/open)))
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
       (catch Throwable _))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(#?(:clj portal.api/clear :cljs portal.web/clear))"))

(defun portal/invoke-portal-command (command-str)
  (cider-nrepl-sync-request:eval
   (concat "(#?(:clj portal.api/eval-str :cljs portal.web/eval-str) \"" command-str "\")")))

(defun portal.ui.commands/select-root ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-root portal.ui.state/state)"))

(defun portal.ui.commands/select-next ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-next portal.ui.state/state)"))

(defun portal.ui.commands/select-prev ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-prev portal.ui.state/state)"))

(defun portal.ui.commands/select-parent ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-parent portal.ui.state/state)"))

(defun portal.ui.commands/select-child ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-child portal.ui.state/state)"))

(defun portal.ui.commands/history-back ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/history-back portal.ui.state/state)"))

(defun portal.ui.commands/history-forward ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/history-forward portal.ui.state/state)"))

(defun portal.ui.commands/focus-selected ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/focus-selected portal.ui.state/state)"))

(defun portal.ui.commands/toggle-expand ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/toggle-expand portal.ui.state/state)"))

;; Not working
(defun portal.ui.commands/copy ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/copy portal.ui.state/state)"))

;; Not working
(defun portal.ui.commands/copy-path ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/copy-path portal.ui.state/state)"))

;; Not working
(defun portal.ui.commands/copy-json ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/copy-json portal.ui.state/state)"))

;; Not working
(defun portal.ui.commands/toggle-selection ()
  (interactive)
  (portal/invoke-portal-command
   "(require '[portal.ui.state :as s])
    (defn toggle-selection! []
      (let [context (s/get-selected-context @s/state)
            selected? (s/selected @s/state context)]
        (s/dispatch!
          s/state
          (if selected
            s/deselect-context
            s/select-context)
          context
          true)))
      (toggle-selection!)"))

(defun portal.ui.commands/set-tree-viewer ()
  (interactive)
  (portal/invoke-portal-command
   "(require '[portal.ui.state :as s])
    (defn set-viewer! [viewer]
      (s/dispatch!
       s/state
       assoc-in
       [:selected-viewers
        (s/get-location
         (s/get-selected-context @s/state))]
       viewer))
    (set-viewer! :portal.viewer/tree)"))


;; This is needed to send the result of cider evaluate to portal
;; (after! cider-mode
;;   (defun cider-tap (&rest r)
;;     (cons (concat "(let [__value "
;;                   (caar r)
;;                   "] (tap> __value) __value)")
;;           (cdar r)))
;;   (advice-add 'cider-nrepl-request:eval
;;               :filter-args #'cider-tap))


(defadvice! nmkip/add-tap (fn &rest args)
  :around #'cider-interactive-eval
  (let* ((form (nth 0 args))
         (bounds (nth 2 args))
         (form  (or form (apply #'buffer-substring-no-properties bounds)))
         (form (concat "(doto " form " tap>)")))
    (apply fn form (cdr args))))

;;(defadvice! nmkip/add-tap (fn &rest args)
;;    :around #'cider-interactive-eval
;;    (let* ((form (nth 0 args))
;;           (bounds (nth 2 args))
;;           (form  (or form (apply #'buffer-substring-no-properties bounds))))
;;      (if (eq major-mode 'clojure-mode)
;;       (let* ((form (concat "(doto " form " tap>)")))
;;         (message form)
;;         (apply fn form (cdr args)))
;;       (progn
;;         (message form)
;;         (apply fn args)))))
