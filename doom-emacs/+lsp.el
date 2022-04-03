(use-package! lsp
  :config
  (let ((lsp-dirs-to-ignore
         '("[/\\\\]\\.cpcache\\'"
           "[/\\\\]\\.datomic\\'"
           "[/\\\\]cljs-runtime\\'"
           "[/\\\\]\\.lsp\\'"
           "[/\\\\]\\.store\\'"
           "[/\\\\]\\.shadow-cljs\\'")))
    (dolist (item lsp-dirs-to-ignore)
      (add-to-list 'lsp-file-watch-ignored-directories item))))

(use-package! lsp-mode
  :commands lsp
  :hook ((clojure-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-enable-file-watchers nil
        lsp-signature-render-documentation t
        lsp-signature-function 'lsp-signature-posframe
        lsp-semantic-tokens-enable t
        lsp-idle-delay 0.3
        lsp-use-plists nil
        lsp-completion-sort-initial-results t ; check if should keep as t
        lsp-completion-no-cache t
        lsp-completion-use-last-result nil)
  (add-hook 'lsp-mode-hook (lambda () (setq-local company-format-margin-function #'company-vscode-dark-icons-margin))))

(use-package! lsp-treemacs
  :config
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package! lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-peek-list-width 60
        lsp-ui-doc-max-width 60
        lsp-ui-doc-enable nil
        lsp-ui-peek-fontify 'always
        lsp-ui-sideline-show-code-actions nil))

(use-package! lsp-java
  :after java-mode
  :config
  (setq lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t))

(setq lsp-ui-sideline-actions-icon nil
      lsp-ui-sideline-show-code-actions nil
      lsp-ui-doc-position 'at-point)
