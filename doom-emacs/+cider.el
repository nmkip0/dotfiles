(use-package! cider
  :after clojure-mode
  :config
  (setq cider-use-xref nil
        cider-ns-refresh-show-log-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-display-in-current-window t
        cider-show-error-buffer t ;'only-in-repl
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil)
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  (set-lookup-handlers! 'cider-mode nil) ; use lsp
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))) ; use lsp)

(setq cider-eldoc-display-for-symbol-at-point nil)
(setq cider-clojure-cli-global-options "-A:portal:dev:1.11")

(defun nmkip/cider-quit-all ()
  "Quit all current CIDER REPLs. Thanks to @plexus"
  (interactive)
  (let ((repls (seq-remove (lambda (r)
                             (equal r (get-buffer "*babashka-repl*")))
                           (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
    (seq-do #'cider--close-connection repls))
  ;; if there are no more sessions we can kill all ancillary buffers
  (cider-close-ancillary-buffers)
  ;; need this to refresh sesman browser
  (run-hooks 'sesman-post-command-hook))

(defun nmkip/cider-eval-sexp-end-of-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (cider-eval-last-sexp)))

(defun nmkip/cider-pprint-eval-sexp-end-of-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (cider-pprint-eval-last-sexp)))

(defun cider-eval-clipboard-handler ()
  (nrepl-make-response-handler
   (current-buffer)
   (lambda (buffer value)
     (with-current-buffer buffer
       (with-temp-buffer
         (insert value)
         (clipboard-kill-region (point-min) (point-max)))))
   (lambda (_buffer out)
     (cider-emit-interactive-eval-output out))
   (lambda (_buffer err)
     (cider-emit-interactive-eval-err-output err))
   '()))

(defun cider-eval-last-sexpr-and-copy-to-clipboard ()
  (interactive)
  (cider-interactive-eval nil
                          (cider-eval-clipboard-handler)
                          (cider-last-sexp 'bounds)
                          (cider--nrepl-pr-request-map)))

(defun k16.dev.system/reset ()
  (interactive)
  (cider-interactive-eval "(k16.dev.system/reset)"))

(provide '+cider)
;;; cider.el ends here
