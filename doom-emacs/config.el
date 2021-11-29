;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq evil-collection-setup-minibuffer t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configur ing packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Local leader
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(setq treemacs-follow-mode  t)
(setq doom-themes-treemacs-theme "all-the-icons")

(define-key evil-motion-state-map (kbd "] e") #'flycheck-next-error)
(define-key evil-motion-state-map (kbd "[ e") #'flycheck-previous-error)

;; Vim keys
(general-define-key :states 'normal "\\" 'evil-snipe-repeat-reverse)

;; Keybindings

;; Packages
(use-package! evil-cleverparens
  :commands evil-cleverparens-mode
  :hook
    (emacs-lisp-mode . evil-cleverparens-mode)
    (clojure-mode . evil-cleverparens-mode)
    (clojurescript-mode . evil-cleverparens-mode)
    (cider-repl-mode . evil-cleverparens-mode)
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  :custom
  (evil-cleverparens-use-s-and-S nil)
  (evil-cleverparens-use-additional-bindings nil)
  (evil-cleverparens-use-additional-movement-keys nil)
  :config
  (map! :map evil-cleverparens-mode-map
        :nv "M-[" 'evil-cp-beginning-of-defun
        :nv "M-]" 'evil-cp-end-of-defun
        :nv "M-{" 'evil-cp-previous-opening
        :nv "M-}" 'evil-cp-next-closing))

(use-package! smartparens
  :commands smartparens-mode
:hook
    (prog-mode . smartparens-mode)
    (emacs-lisp-mode . smartparens-strict-mode)
    (clojure-mode . smartparens-strict-mode)
    (clojurescript-mode . smartparens-strict-mode)
    (cider-repl-mode . smartparens-strict-mode)
  :config
    (sp-local-pair sp-lisp-modes "'" nil :actions nil)
    (sp-local-pair sp-lisp-modes "`" nil :actions nil)
    (show-smartparens-global-mode))

;; Projectile

(after! projectile
  (setq projectile-sort-order 'recently-active))

;; Clojure;; clojure

(use-package! cider
  :after clojure-mode
  :config
  (setq cider-ns-refresh-show-log-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-display-in-current-window t
        cider-show-error-buffer t ;'only-in-repl
        cider-font-lock-dynamically nil ; use lsp semantic tokens
        cider-eldoc-display-for-symbol-at-point nil ; use lsp
        cider-prompt-for-symbol nil)
  (set-popup-rule! "*cider-test-report*" :side 'right :width 0.4)
  (set-popup-rule! "^\\*cider-repl" :side 'bottom :quit nil)
  (set-lookup-handlers! 'cider-mode nil) ; use lsp
  (add-hook 'cider-mode-hook (lambda () (remove-hook 'completion-at-point-functions #'cider-complete-at-point))) ; use lsp
  )

(after! clojure-mode
  (setq clojure-toplevel-inside-comment-form t))

(setq lsp-ui-sideline-actions-icon nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-position 'at-point)

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

(defun cider-eval-last-sexpr-and-copy-to-clipboard ()
  (interactive)
  (cider-interactive-eval nil
                         (cider-eval-clipboard-handler)
                         (cider-last-sexp 'bounds)
                         (cider--nrepl-pr-request-map)))

(map! :after cider-mode
      :map cider-mode-map
      :localleader
      :prefix "e"
      :desc "Cider eval last sexpr and copy to clipboard"
      "c" #'cider-eval-last-sexpr-and-copy-to-clipboard
      "l" #'nmkip/cider-eval-sexp-end-of-line
      "v" #'cider-eval-sexp-at-point)

;; TODO: Review these keys.
(map! :map cider-inspector-mode-map
      :n "h" 'cider-inspector-pop
      :n "H" 'cider-inspector-prev-page
      :n "j" 'cider-inspector-next-inspectable-object
      :n "k" 'cider-inspector-previous-inspectable-object
      :n "l" 'cider-inspector-operate-on-point
      :n "L" 'cider-inspector-next-page
      :n "q" 'quit-window
      :n "r" 'cider-inspector-refresh
      :n "s" 'cider-inspector-set-page-size
      :n (kbd "RET") 'cider-inspector-operate-on-point
      :n [mouse-1] 'cider-inspector-operate-on-click)

;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api) (portal.api/tap) (portal.api/open)"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Example key mappings for doom emacs
(map! :localleader
      :map (clojure-mode-map clojurescript-mode-map)
      :prefix "P"
      :n "o" #'portal.api/open
      :n "l" #'portal.api/clear
      :n "q" #'portal.api/close)

;; NOTE: You do need to have portal on the class path and the easiest way I know
;; how is via a clj user or project alias.
(setq cider-clojure-cli-global-options "-A:portal")
(setq cider-eldoc-display-for-symbol-at-point nil)

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

(defadvice! nmkip/add-tap (fn &rest args)
    :around #'cider-interactive-eval
    (let* ((form (nth 0 args))
           (bounds (nth 2 args))
           (form  (or form (apply #'buffer-substring-no-properties bounds)))
           (form (concat "(doto " form " tap>)")))
      (apply fn form (cdr args))))

(defadvice! nmkip/find-file-and-select (&optional _)
    :after #'treemacs-find-file
    (treemacs-select-window))

(defhydra nmkip/hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "scale up")
  ("j" text-scale-decrease "scale down")
  ("0" doom/reset-font-size "reset font")
  ("q" nil "quit" :exit t))

(map! :leader
      :prefix ("z" . "zoom")
      "." #'nmkip/hydra-text-scale/body
      :desc "Increase font size"
      "j" #'nmkip/hydra-text-scale/text-scale-decrease
      :desc "Decrease font size"
      "k" #'nmkip/hydra-text-scale/text-scale-increase
      :desc "Reset font size"
      "0" #'nmkip/hydra-text-scale/doom/reset-font-size)

(defhydra nmkip/hydra-window (:hint nil)
  "
          Split: _v_ert  _s_:horz
         Delete: _d_:delete  _o_:other windows
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
         Resize: _e_: enlargen _<_:decrease width left  _>_:increase width _-_:decrease height  _+_:increase height  _=_: balance
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("p" previous-buffer)
  ("n" next-buffer)
  ("b" switch-to-buffer)
  ("f" find-file)

  ("s" split-window-below)
  ("v" split-window-right)

  ("d" delete-window)
  ("o" delete-other-windows)

  ("e" doom/window-enlargen)
  ("<" evil-window-decrease-width)
  ("-" evil-window-decrease-height)
  ("+" evil-window-increase-height)
  (">" evil-window-increase-width)
  ("=" balance-windows)

  ("q" nil))

(map! :leader
      :prefix "w"

      :desc "hydra"
      "." #'nmkip/hydra-window/body

      :desc "doom/window-enlargen"
      "<" #'nmkip/hydra-window/doom/window-enlargen

      :desc "evil-window-decrease-width"
      "<" #'nmkip/hydra-window/evil-window-decrease-width

      :desc "evil-window-decrease-height"
      "-" #'nmkip/hydra-window/evil-window-decrease-height

      :desc "evil-window-increase-height"
      "+" #'nmkip/hydra-window/evil-window-increase-height

      :desc "evil-window-increase-width"
      ">" #'nmkip/hydra-window/evil-window-increase-width
      )

(after! avy
  (setq avy-all-windows t))

(defhydra nmkip/hydra-buffer (:hint nil)
  "
 _d_: kill buffer _p_revious  _n_ext  _b_:select
"
  ("p" previous-buffer)
  ("n" next-buffer)
  ("b" switch-to-buffer)
  ("d" kill-this-buffer)

  ("q" nil))

(map! :leader
      :prefix "b"
      :desc "hydra"
      "." #'nmkip/hydra-buffer/body)

(map! :nv "M-r" #'evil-multiedit-match-all)

(after! ranger
  (ranger-override-dired-mode t))

;; Command log
(map! :leader
      (:prefix "t"
        :desc "Global Command Log"
        "L" #'global-command-log-mode)
      (:prefix "b"
       :desc "Toggle Command Log buffer"
       "L" #'clm/toggle-command-log-buffer))



(defun jet-pretty ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(setq auth-sources '("~/.authinfo"))

(use-package! treemacs-all-the-icons
  :after treemacs)

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
        company-quickhelp-max-lines 15
        ))

(after! company
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

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
