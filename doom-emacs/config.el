;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme 'doom-one)
(setq org-directory "~/org/")
(setq display-line-numbers-type 'relative)
(setq evil-collection-setup-minibuffer nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configur ing packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
            sql-mode         ; sqlformat is currently broken
            tex-mode         ; latexindent is broken
            js-mode
            latex-mode))

(use-package! treemacs-all-the-icons
  :after treemacs)

(setq treemacs-follow-mode  t)
(setq doom-themes-treemacs-theme "all-the-icons")

(after! projectile
  (setq projectile-sort-order 'recently-active))

(defadvice! nmkip/find-file-and-select (&optional _)
  :after #'treemacs-find-file
  (treemacs-select-window))

(after! avy
  (setq avy-all-windows t))

(after! ranger
  (ranger-override-dired-mode t))

(setq auth-sources '("~/.authinfo"))

(use-package! company
  :config
  (setq company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend)))

(use-package! company-quickhelp
  :init
  (company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 1
        company-quickhelp-use-propertized-text t
        company-quickhelp-max-lines 15))

(after! clojure-mode
  (setq clojure-toplevel-inside-comment-form t))

(use-package! clj-refactor
  :after clojure-mode
  :config
  (set-lookup-handlers! 'clj-refactor-mode nil)
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-add-ns-to-blank-clj-files nil ; use lsp
        cljr-magic-require-namespaces
        '(("pp" . "clojure.pprint"))))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(load! "+bindings")
(load! "+parens")
(load! "+lsp")
(load! "+functions")
(load! "+cider")
(load! "+portal")

(consult-customize
   +default/search-project
   :preview-key '(:debounce 0.2 any))

;;; config.el ends here
