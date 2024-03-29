;; package --- Summary
;;; Commentary:

;;; Code:
;; (def user/last-viewer (atom {:current :portal.viewer/inspector
;;                             :viewers []}))

;; TODO Symlink cljc files. Base path var.
(defvar open-portal (f-read-text "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/open_portal.cljc"))
(defvar open-portal-logger (f-read-text "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/logger/user.clj"))
(defvar connect-remote-jvm (f-read-text "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/remote/jvm/user.clj"))
(defvar connect-remote-node (f-read-text "/home/nmkip/dotfiles/nmkip/doom-emacs/portal/remote/node/user.cljs"))

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval open-portal))

(defun portal.api/open-logger ()
  (interactive)
  (cider-nrepl-sync-request:eval open-portal-logger))

(defun portal.api/connect-remote-jvm ()
  (interactive)
  (cider-nrepl-sync-request:eval connect-remote-jvm))

(defun portal.api/connect-remote-node ()
  (interactive)
  (cider-nrepl-sync-request:eval connect-remote-node))

(defun portal/run-ns-test ()
  (interactive)
  (if-let (ns-str (cider-current-ns t))
      (let* ((suffix "-test")
             (ns-str (if (string-suffix-p suffix ns-str)
                         ns-str
                       (concat ns-str suffix)))
             (req (concat "(run-ns-tests \"" ns-str "\")")))
        (message req)
        (cider-nrepl-sync-request:eval req))
    (message "no ns")))

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

(defun portal.web/eval-str (command-str)
  (cider-nrepl-sync-request:eval
   (concat "(portal.web/eval-str \"" command-str "\")")))

(defun portal.ui.commands/select-next-viewer ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-next-viewer portal.ui.state/state)"))

(defun portal.ui.commands/select-prev-viewer ()
  (interactive)
  (portal/invoke-portal-command
   "(portal.ui.commands/select-next-viewer portal.ui.state/state)"))

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

(defun portal.ui.commands/copy ()
  (interactive)
  (with-temp-buffer
    (insert (nrepl-dict-get (cider-nrepl-sync-request:eval "@user/portal") "value"))
    (jet-pretty-region (point-min) (point-max))
    (copy-region-as-kill (point-min) (point-max))))

(defun portal.ui.commands/copy-path ()
  (interactive)
  (with-temp-buffer
    (insert (nrepl-dict-get (portal/invoke-portal-command
                             "(require '[portal.ui.state :as s])
                              (vec (rest (s/get-path @s/state)))")
                            "value"))
    (copy-region-as-kill (point-min) (point-max))))

(defun portal.ui.commands/copy-json ()
  (interactive)
  (with-temp-buffer
    (insert (nrepl-dict-get (cider-nrepl-sync-request:eval "@user/portal") "value"))
    (jet-pretty-json-region (point-min) (point-max))
    (copy-region-as-kill (point-min) (point-max))))

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
          (if selected?
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

(defun portal.ui.commands/cycle-viewer ()
  (interactive)
  (insert (nrepl-dict-get
           (portal/invoke-portal-command
            "(require '[portal.ui.inspector :as ins])
             (require '[portal.ui.state :as s])
             (defn get-compatible-viewers []
               (when-let [selected-context (s/get-selected-context @s/state)]
                 (let [viewers (ins/get-compatible-viewers @ins/viewers selected-context)]
                   (mapv :name viewers))))
             (get-compatible-viewers)"
            )
           "value")))


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

;; (after! cider-mode
;;   (defun cider-tap (&rest r)
;;     (cons (concat "(let [__value "
;;                   (caar r)
;;                   "] (tap> (if (instance? clojure.lang.IObj __value)
;;                              (with-meta __value {:portal.viewer/default :portal.viewer/pprint})
;;                              __value))
;;                      __value)")
;;           (cdar r)))

;;   (advice-add 'cider-nrepl-request:eval
;;               :filter-args #'cider-tap))

 ;; (dev/start! {:report portal.malli.visualizer/display-malli-error})

(defun med/cider-eval-on-top-level-form (fn-str)
  (let ((quoted-defn (concat "'" (cider-defun-at-point))))
    (cider-interactive-eval (concat "(" fn-str " " quoted-defn ")"))))

(defun malli-check-this ()
  (interactive)
  (med/cider-eval-on-top-level-form
   "#(portal.malli.visualizer/check->portal (mi/check {:filters [(mi/-filter-var #{(resolve (second %))})]}))"))

(defun malli-check-all ()
  (interactive)
  (cider-interactive-eval "(-> (mi/check) portal.malli.visualizer/check->portal)"))
