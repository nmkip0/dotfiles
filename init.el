;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; Path to this file
(defvar emacs-org-config-path "~/dotfiles/nmkip/Emacs.org")

;; You will most likely need to adjust this font size for your system!
(defvar nmkip/default-font-size 120)
(defvar nmkip/fixed-pitch-font-size 120)
(defvar nmkip/variable-pitch-font-size 150)

;; Lisp modes
(defconst nmkip/lisp-modes '(cider-repl-mode
                            clojure-mode
                            clojurec-mode
                            clojurescript-mode
                            clojurex-mode
                            emacs-lisp-mode
                            eshell-mode
                            inf-clojure-mode
                            inferior-emacs-lisp-mode
                            lisp-interaction-mode
                            lisp-mode))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

(setq display-line-numbers-type 'relative) ; Relative line numbers

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height nmkip/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height nmkip/fixed-pitch-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height nmkip/variable-pitch-font-size :weight 'regular)

(use-package hydra
   :ensure t)

;; Buffers
(defun nmkip/swap-buffers-to-window (windownum follow-focus-p)
      "Swaps visible buffers between active window and selected window.
      follow-focus-p controls whether focus moves to new window (with buffer), or
      stays on current"
      (interactive)
      (if (> windownum (length (window-list-1 nil nil t)))
          (message "No window numbered %s" windownum)
        (let* ((b1 (current-buffer))
               (w1 (selected-window))
               (w2 (winum-get-window-by-number windownum))
               (b2 (window-buffer w2)))
          (unless (eq w1 w2)
            (set-window-buffer w1 b2)
            (set-window-buffer w2 b1)
            (unrecord-window-buffer w1 b1)
            (unrecord-window-buffer w2 b2)))
        (when follow-focus-p (winum-select-window-by-number windownum))))

;; define and evaluate numbered functions:
;; buffer-to-window-1 to 9
(dotimes (i 9)
(let ((n (+ i 1)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
            ,(format "Move buffer to the window with number %i." n)
            (interactive "P")
            (nmkip/swap-buffers-to-window ,n t)))))

(defun nmkip/show-messages-buffer ()
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun nmkip/show-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Windows
(defun nmkip/maximize-window ()
  "Maximize window"
  (interactive)
  (save-excursion
      (if (and (= 1 (length (window-list)))
               (assoc ?_ register-alist))
          (jump-to-register ?_)
          (progn
              (window-configuration-to-register ?_)
              (delete-other-windows)))))

;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (use-package general
    :config
    (general-create-definer nmkip/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (general-create-definer nmkip/local-leader-keys
      :prefix ",")

    (general-define-key
     :states 'normal
     "\\" 'evil-repeat-find-char-reverse)

    (nmkip/leader-keys
      "b"  '(:ignore t :which-key "buffers")
      "f"  '(:ignore t :which-key "files")
      "g"  '(:ignore t :which-key "git")
      "h"  '(:ignore t :which-key "help")
      "k"  '(:ignore t :which-key "lisp")
      "s"  '(:ignore t :which-key "search")
      "w"  '(:ignore t :which-key "windows")
      "t"  '(:ignore t :which-key "toggles")

      "SPC" '(counsel-M-x :which-key "M-x")
      "TAB" '(evil-switch-to-windows-last-buffer :which-key "Last buffer")
      "/" '(swiper :which-key "swiper")
      "!" '(eshell :which-key "eshel")
      "1" '(winum-select-window-1 :which-key "Select window 1")
      "2" '(winum-select-window-2 :which-key "Select window 2")
      "3" '(winum-select-window-3 :which-key "Select window 3")
      "4" '(winum-select-window-4 :which-key "Select window 4")
      "5" '(winum-select-window-5 :which-key "Select window 5")
      "6" '(winum-select-window-6 :which-key "Select window 6")
      "7" '(winum-select-window-7 :which-key "Select window 7")
      "8" '(winum-select-window-8 :which-key "Select window 8")
      "9" '(winum-select-window-9 :which-key "Select window 9")
      "T" '(counsel-load-theme  :which-key "Load theme")

      ;; Buffers
      "b1" '(buffer-to-window-1 :which-key "Move buffer to window 1")
      "b2" '(buffer-to-window-2 :which-key "Move buffer to window 2")
      "b3" '(buffer-to-window-3 :which-key "Move buffer to window 3")
      "b4" '(buffer-to-window-4 :which-key "Move buffer to window 4")
      "b5" '(buffer-to-window-5 :which-key "Move buffer to window 5")
      "b6" '(buffer-to-window-6 :which-key "Move buffer to window 6")
      "b7" '(buffer-to-window-7 :which-key "Move buffer to window 7")
      "b8" '(buffer-to-window-8 :which-key "Move buffer to window 8")
      "b9" '(buffer-to-window-9 :which-key "Move buffer to window 9")
      "bl" '(clm/open-command-log-buffer :which-key "Command log buffer")
      "bb" '(counsel-switch-buffer :which-key "Switch buffer")
      "bd" '(kill-this-buffer :which-key "Delete buffer")
      "bD" '(kill-buffer-and-window :which-key "Delete buffer and window")
      "be" '(flycheck-list-errors :which-key "Flycheck list errors")
      "bm" '(nmkip/show-messages-buffer :which-key "Messages buffer")
      "bs" '(nmkip/show-scratch-buffer :which-key "Scratch buffer")
      "br" '(Revert buffer :which-key "Revert buffer")

      ;; Files
      "ff" '(counsel-find-file :which-key "Find file")
      "fs" '(save-buffer :which-key "Save file")
      "fS" '(evil-write-all :which-key "Save all files")
      "fr" '(counsel-recentf :which-key "Recent files")
      "fR" '(rename-file :which-key "Rename file")

      ;; help
      "ha" '(counsel-apropos :which-key "Apropos")
      "hb" '(counsel-describe-binds :which-key "Describe binds")
      "hf" '(counsel-describe-function :which-key "Describe function")
      "hF" '(counsel-describe-face :which-key "Describe face")
      "hh" '(helpful-at-point :which-key "Helpul at point")
      "hk" '(describe-key :which-key "Describe key")
      "hm" '(describe-mode :which-key "Describe mode")
      "hp" '(describe-package :which-key "Describe package")
      "hs" '(counsel-symbol :which-key "Describe symbol")
      "ht" '(describe-theme :which-key "Describe theme")
      "hv" '(counsel-describe-variable :which-key "Describe variable")

      "ss" '(swiper :which-key "Search")
      "sd" '(counsel-rg :which-key "Search in directory")

      ;; Windows
      "w=" '(balance-windows :which-key "Balance windows")
      "wd" '(delete-window :which-key "Delete window")
      "wD" '(delete-other-windows :which-key "Delete other windows")
      "wh" '(evil-window-left :which-key "Focus window left")
      "wH" '(evil-window-move-far-left :which-key "Move far left")
      "wj" '(evil-window-down :which-key "Focus window down")
      "wJ" '(evil-window-move-very-bottom :which-key "Move very bottom")
      "wk" '(evil-window-up :which-key "Focus window up")
      "wK" '(evil-window-move-very-top :which-key "Move very top")
      "wl" '(evil-window-right :which-key "Focus window right")
      "wL" '(evil-window-move-far-right :which-key "Move far right")
      "wm" '(nmkip/maximize-window :which-key "Maximize window")
      "wr" '(winner-redo :which-key "Winner redo")
      "ws" '(split-window-below :which-key "Split horizontally")
      "wu" '(winner-undo :which-key "Winner undo")
      "wv" '(split-window-right :which-key "Split vertically")

      ;; Toggles
      "td" '(diff-hl-mode :whick-key "Diff-hl mode")
      "tf" '(flycheck-mode :whick-key "Flycheck mode")
      "tl" '(command-log-mode :whick-key "Command log mode")
      "tL" '(global-command-log-mode :whick-key "Command log mode")
      "ts" '(smartparens-mode :whick-key "Smartparens mode")
      "tS" '(smartparens-strict-mode :whick-key "Smartparens strict mode")
      ))

(defhydra hydra-buffers (:hint nil)
    "
Buffer Transient State
[_k_]: kill buffer [_n_]: next buffer [_p_/_N_]: previous buffer "
    ("k" kill-current-buffer)
    ("n" next-buffer)
    ("N" previous-buffer)
    ("p" previous-buffer)
    ("q" nil "quit" :color blue))

(nmkip/leader-keys
    "b." '(hydra-buffers/body :which-key "Buffer transient state")
    "b," '(hydra-buffers/previous-buffer :which-key "Buffer transient state")
    "bn" '(hydra-buffers/next-buffer :which-key "Next buffer")
    "bN" '(hydra-buffers/previous-buffer :which-key "Previous buffer")
    "bp" '(hydra-buffers/previous-buffer :which-key "Previous buffer")
)

(defhydra hydra-windows (:hint nil)
    "Window Transient State
    "
    ("[" shrink-window-horizontally "shrink" :column "horizontal")
    ("]" enlarge-window-horizontally "enlarge" :column "horizontal")
    ("{" shrink-window "shrink" :column "vertical")
    ("}" enlarge-window "enlarge" :column "vertical")
    ("q" nil "quit" :color blue :column nil)
)

(nmkip/leader-keys
    "w." '(hydra-windows/body :which-key "Windows transient state")
    "w[" '(hydra-windows/shrink-window-horizontally :which-key "Shrink window horizontally")
    "w]" '(hydra-windows/enlarge-window-horizontally :which-key "Enlarge window horizontally")
    "w{" '(hydra-windows/shrink-window :which-key "Shrink window vertically")
    "w}" '(hydra-windows/enlarge-window :which-key "Enlarge window vertically"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions in visual-line-mode buffers
  (general-def 'normal 'visual-line-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package winum
    :config (winum-mode))

(use-package winner
    :ensure nil
    :config (winner-mode 1))

(use-package command-log-mode)

(use-package doom-themes)
  ;;:init (load-theme 'doom-vibrant t))

(use-package solarized-theme
  :init
  (setq solarized-use-less-bold t)
 :config
 (load-theme 'solarized-dark-high-contrast t) )

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-d" . ivy-scroll-up-command)
         ("C-h" . ivy-backward-delete-char)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("C-u" . ivy-scroll-down-command)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))



(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "scale up")
  ("j" text-scale-decrease "scale down")
  ("0" text-scale-set "reset font")
  ("q" nil "quit" :exit t))

(nmkip/leader-keys
  "z" '(hydra-text-scale/body :which-key "scale text"))

(defun nmkip/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun nmkip/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . nmkip/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (nmkip/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun nmkip/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . nmkip/org-mode-visual-fill))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; Automatically tangle our Emacs.org config file when we save it
(defun nmkip/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name emacs-org-config-path))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'nmkip/org-babel-tangle-config)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(defhydra hydra-flycheck (:hint nil)
  "
Flycheck Transient State
[_n_]: next error [_p_/_N_]: previous error "
  ("n" flycheck-next-error)
  ("N" flycheck-previous-error)
  ("p" flycheck-previous-error)
  ("q" nil "quit" :color blue))

(nmkip/leader-keys
  "e" '(:ignore t :which-key "errors")

  "e." '(hydra-flycheck/body :which-key "Flycheck transient state")
  "el" '(flycheck-list-errors :which-key "List errors")
  "en" '(hydra-flycheck/flycheck-next-error :which-key "Next error")
  "eN" '(hydra-flycheck/flycheck-previous-error :which-key "Previous error")
  "ep" '(hydra-flycheck/flycheck-previous-error :which-key "Next error")
  "ev" '(flycheck-verify-setup :which-key "Verify setup")
  "ev" '(flycheck-verify-setup :which-key "Verify setup")
  )

(defhydra hydra-lisp ()
 "Lisp State"
  ("b" sp-forward-barf-sexp "forward barf")
  ("B" sp-backward-barf-sexp "backward barf")
  ("s" sp-forward-slurp-sexp "forward slurp")
  ("S" sp-backward-slurp-sexp "backward slurp")
  ("h" evil-cp-backward-symbol-begin "backward symbol")
  ("j" evil-cp-next-closing "next closing")
  ("k" evil-cp-previous-opening "previous opening")
  ("l" evil-cp-forward-symbol-begin "forward symbol")
  ("r" sp-raise-sexp "raise sexp")
  ("t" sp-transpose-sexp "transpose sexp")
  ("w" sp-rewrap-sexp "rewrap sexp")
  ("y" sp-copy-sexp "copy sexp")
  ("[" sp-wrap-square "wrap []")
  ("(" sp-wrap-round "wrap ()")
  ("{" sp-wrap-curly "wrap {}")
  ("q" nil "quit" :exit t))

(nmkip/local-leader-keys 'normal '(emacs-lisp-mode-map
                                   clojure-mode-map
                                   cider-repl-mode-map
                                   lisp-interaction-mode-map)
  "," '(hydra-lisp/body :which-key "Lisp transient state"))

(nmkip/leader-keys 'normal '(emacs-lisp-mode-map
                             clojure-mode-map
                             cider-repl-mode-map
                             lisp-interaction-mode-map)

  "k." '(hydra-lisp/body :which-key "Lisp transient state")
  "kb" '(hydra-lisp/sp-forward-barf-sexp :which-key "forward barf")
  "kB" '(hydra-lisp/sp-backward-barf-sexp :which-key "backward barf")
  "ks" '(hydra-lisp/sp-forward-slurp-sexp :which-key "forward slurp")
  "kS" '(hydra-lisp/sp-backward-slurp-sexp :which-key "backward slurp")
  "kh" '(hydra-lisp/evil-cp-backward-symbol-begin :which-key "backward symbol")
  "kj" '(hydra-lisp/evil-cp-next-closing :which-key "next closing")
  "kk" '(hydra-lisp/evil-cp-previous-opening :which-key "previous opening")
  "kl" '(hydra-lisp/evil-cp-forward-symbol-begin :which-key "forward symbol")
  "kr" '(hydra-lisp/sp-raise-sexp :which-key "raise sexp")
  "kt" '(hydra-lisp/sp-transpose-sexp :which-key "transpose sexp")
  "kw" '(hydra-lisp/sp-rewrap-sexp :which-key "rewrap sexp")
  "ky" '(hydra-lisp/sp-copy-sexp :which-key "copy sexp")
  "k[" '(hydra-lisp/sp-wrap-square :which-key "wrap []")
  "k(" '(hydra-lisp/sp-wrap-round :which-key "wrap ()")
  "k{" '(hydra-lisp/sp-wrap-curly :which-key "wrap {}"))

(defun nmkip/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :hook (clojure-mode . nmkip/clojure-mode-hook)
  :ensure t
  :config (require 'flycheck-clj-kondo))

(defun nmkip/cider-config ()
  (setq cider-repl-pop-to-buffer-on-connect nil)
  ;(evil-collection-cider-setup)
)

(defun nmkip/cider-inspector-mode-hook ()
  (general-override-local-mode)
  (general-define-key
   :states 'normal
   :keymaps 'local
   "h" 'cider-inspector-pop
   "H" 'cider-inspector-prev-page
   "j" 'cider-inspector-next-inspectable-object
   "k" 'cider-inspector-previous-inspectable-object
   "l" 'cider-inspector-operate-on-point
   "L" 'cider-inspector-next-page
   "q" 'quit-window
   "r" 'cider-inspector-refresh
   "s" 'cider-inspector-set-page-size
   (kbd "RET") 'cider-inspector-operate-on-point
   [mouse-1] 'cider-inspector-operate-on-click
   ))

(defun nmkip/cider--debug-mode-hook ()
  (if (bound-and-true-p cider--debug-mode)
      (progn
        (general-override-local-mode 1)
        (general-define-key
         :states 'normal
         :predicate '((lambda () (bound-and-true-p cider--debug-mode)))
         :keymaps 'local
         "n" 'evil-collection-cider-debug-next
         "c" 'evil-collection-cider-debug-continue
         "o" 'evil-collection-cider-debug-out
         "q" 'evil-collection-cider-debug-quit
         "e" 'evil-collection-cider-debug-eval
         "J" 'evil-collection-cider-debug-inject
         "p" 'evil-collection-cider-debug-inspect
         "L" 'evil-collection-cider-debug-locals
         "H" 'cider-debug-move-here))
    (general-override-local-mode 0)))

(use-package cider
  :ensure t
  :hook
  (cider-inspector-mode . nmkip/cider-inspector-mode-hook)
  (cider--debug-mode . nmkip/cider--debug-mode-hook)
  :config (nmkip/cider-config))

(use-package clj-refactor)

(use-package html-to-hiccup
  :ensure t)

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

(nmkip/local-leader-keys '(normal visual) clojure-mode-map
  "=" '(:ignore t :which-key "format")
  "=e" '(:ignore t :which-key "edn")
  "d" '(:ignore t :which-key "debug")
  "e" '(:ignore t :which-key "evaluation")
  "e;" '(:ignore t :which-key "comment")
  "h" '(:ignore t :which-key "help")
  "r" '(:ignore t :which-key "refactor")
  "t" '(:ignore t :which-key "tests")
  "T" '(:ignore t :which-key "toggle")

  "'" 'sesman-start 

  ;; Format
  "==" '(cider-format-buffer :which-key "Format buffer")
  "=f" '(cider-format-defun :which-key "Format function")
  "=r" '(cider-format-region :which-key "Format region")

  ;; Format edn
  "=eb" '(cider-format-edn-buffer :which-key "Format edn buffer")
  "=ee" '(cider-format-edn-last-sexp :which-key "Format edn last sexp")
  "=er" '(cider-format-edn-region :which-key "Format edn region")

  ;; Debug
  "df" '(cider-debug-defun-at-point :which-key "Debug function")
  "di" '(cider-inspect :which-key "Cider inspect")
  "dI" '(cider-inspect-last-result :which-key "Cider inspect last result")

  ;; Evaluation
  "e$" '(nmkip/cider-eval-sexp-end-of-line :which-key "Eval line")
  "eb" '(cider-eval-buffer :which-key "Eval buffer")
  "ee" '(cider-eval-last-sexp :which-key "Eval sexp before point")
  "ef" '(cider-eval-defun-at-point :which-key "Eval function")
  "el" '(nmkip/cider-eval-sexp-end-of-line :which-key "Eval line")
  "em" '(cider-macroexpand-1 :which-key "Macroexpand")
  "eM" '(cider-macroexpand-all :which-key "Macroexpand all")
  "ev" '(cider-eval-sexp-at-point :which-key "Eval sexp at point")
  "eL" '(nmkip/cider-pprint-eval-sexp-end-of-line :which-key "Eval pprint line")
  "eF" '(cider-pprint-eval-defun-at-point :which-key "Eval pprint function")

  ;; Evaluation to comment
  "e;f" '(cider-eval-defun-to-comment :which-key "Eval function")
  "e;F" '(cider-pprint-eval-defun-to-comment :which-key "Eval pprint function")

  ;; Help
  "ha" '(cider-apropos :which-key "Apropos")
  "hd" '(cider-clojuredocs :which-key "Clojuredocs")
  "hh" '(cider-doc :which-key "Doc")
  "hj" '(cider-javadoc :which-key "Javadoc")
  "hn" '(cider-brose-ns :which-key "Browse ns")
  "hr" '(cljr-describe-refactoring :which-key "Describe refactoring")
  "hs" '(cider-browse-spec :which-key "Browse spec")
  "hS" '(cider-browse-spec-all :which-key "Browse all specs")

  "ra" '(:ignore t :which-key "Add")
  "rai" '(cljr-add-import-to-ns :which-key "Add import")
  "ral" '(cljr-introduce-let :which-key "Add let")
  "ram" '(cljr-add-missing-libspec :which-key "Add missing dependency")
  "rap" '(cljr-add-project-dependency :which-key "Add project dependency")
  "rar" '(cljr-add-require-to-ns :which-key "Add require")
  "ras" '(cljr-add-stubs :which-key "Add stubs for protocol at point")

  "rc" '(:ignore t :which-key "Convert")
  "rch" '(html-to-hiccup-convert-region :which-key "Region to hiccup")

  "rh" '(cljr-describe-refactoring :which-key "Describe refactoring")

  "rf" '(:ignore t :which-key "Function")
  "rfc" '(cljr-create-fn-from-example :which-key "Create")
  "rfp" '(cljr-promote-function :which-key "Promote/Extract")

  "ri" '(:ignore t :which-key "Inline")
  "ril" '(cljr-remove-let :which-key "Remove let and inline")
  "ris" '(cljr-inline-symbol :which-key "Inline symbol")

  "rr" '(cljr-rename-symbol :which-key "Rename symbol")

  "rt" '(:ignore t :which-key "Thread")
  "rtf" '(cljr-thread-first-all :which-key "Thread first all")
  "rti" '(cljr-thread :which-key "Thread into current thread")
  "rtl" '(cljr-thread-last-all :which-key "Thread last all")
  "rtu" '(cljr-unwind-all :which-key "Unwind entire thread")
  "rtU" '(cljr-unwind :which-key "Unwind one thread step")

  ;; Tests
  "ta" '(cider-test-run-project-tests :which-key "Run all")
  "tb" '(cider-test-show-report :which-key "Show report")
  "tl" '(cider-test-run-loaded-tests :which-key "Run all loaded")
  "tn" '(cider-test-run-ns-tests :which-key "Run ns")
  "tr" '(cider-test-rerun-failed-tests :which-key "Run failed")
  "tt" '(cider-test-run-test :which-key "Run test at point")

  ;; Toggle
  "Te" '(cider-enlighten-mode :which-key "Cider enlighten mode")
  "Tt" '(cider-auto-test-mode :which-key "Cider auto-test mode")
 )

(defun nmkip/elisp-eval-sexp-end-of-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (eval-last-sexp nil)))

(nmkip/local-leader-keys '(normal visual) '(emacs-lisp-mode-map lisp-interaction-mode-map)
  "=" '(:ignore t :which-key "format")
  "d" '(:ignore t :which-key "debug")
  "e" '(:ignore t :which-key "evaluation")
  "h" '(:ignore t :which-key "help")
  "r" '(:ignore t :which-key "refactor")
  "t" '(:ignore t :which-key "tests")
  "T" '(:ignore t :which-key "toggle")

  "'" 'ielm

  ;; Evaluation
  "e$" '(nmkip/elisp-eval-sexp-end-of-line :which-key "Eval line")
  "eb" '(eval-buffer :which-key "Eval buffer")
  "ee" '(eval-last-sexp :which-key "Eval sexp before point")
  "ef" '(eval-defun :which-key "Eval function")
  "el" '(nmkip/elisp-eval-sexp-end-of-line :which-key "Eval line")
  "er" '(eval-region :which-key "Eval region")

  ;; Help
  "hh" '(helpful-at-point :which-key "Helpful doc")
 )

(use-package company
  :hook
  (clojure-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  (lisp-interaction-mode . company-mode)
  :bind (:map company-active-map
              ("C-w" . evil-delete-backward-word)
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              ("C-l" . company-complete-selection)
              ("<return>" . company-complete-selection)
              ("<tab>" . company-complete-common-or-cycle))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-show-numbers t))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Maybe this
;;("<tab>" . company-indent-or-complete-common)

;; Workaround evil-mode had higher priority when company-mode was active.
(with-eval-after-load 'company
  (add-hook 'evil-local-mode-hook
            (lambda ()
              ;; Note:
              ;; Check if `company-emulation-alist' is in
              ;; `emulation-mode-map-alists', if true, call
              ;; `company-ensure-emulation-alist' to ensure
              ;; `company-emulation-alist' is the first item of
              ;; `emulation-mode-map-alists', thus has a higher
              ;; priority than keymaps of evil-mode.
              ;; We raise the priority of company-mode keymaps
              ;; unconditionally even when completion is not
              ;; activated. This should not cause problems,
              ;; because when completion is activated, the value of
              ;; `company-emulation-alist' is ((t . company-my-keymap)),
              ;; when completion is not activated, the value is ((t . nil)).
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist)))))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
  (projectile-globally-ignored-file-suffixes '("~" "#"))
  (projectile-indexing-method 'hybrid)
  (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;;(when (file-directory-p "~/projects")
    ;;(setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(defhydra hydra-projectile (:hint nil)
    "
Projectile Transient State
[_n_]: next buffer [_p_/_N_]: previous buffer "
    ("n" projectile-next-project-buffer)
    ("N" projectile-previous-project-buffer)
    ("p" projectile-previous-project-buffer)
    ("q" nil "quit" :color blue))

  (nmkip/leader-keys
    "p" '(:ignore t :which-key "projects")

    "p." '(hydra-projectile/body :which-key "Projectile transient state")
    "p*" '(projectile-clear-known-projects :which-key "clean-up")
    "p!" '(projectile-run-shell :which-key "shell")
    "pA" '(projectile-add-known-project :which-key "add project")
    "pb" '(counsel-projectile-switch-to-buffer :which-key "buffers")
    "pd" '(counsel-projectile-find-dir :which-key "find dirs")
    "pD" '(projectile-discover-projects-in-directory :which-key "discover")
    "pf" '(counsel-projectile-find-file :which-key "find file")
    "pK" '(projectile-kill-buffers :which-key "kill buffers")
    "pl" '(projectile-edit-dir-locals :which-key "dir locals")
    "pn" '(hydra-projectile/projectile-next-project-buffer :which-key "next buffer")
    "pN" '(hydra-projectile/projectile-previous-project-buffer :which-key "previous buffer")
    "ps" '(counsel-projectile-rg :which-key "search")
    "pS" '(projectile-multi-occur :which-key "multi-occur")
    "pp" '(counsel-projectile-switch-project :which-key "switch project")
    "pr" '(projectile-recentf :which-key "recent files")
    "pt" '(projectile-toggle-between-implementation-and-test :which-key "implementation<->test")
    "pT" '(projectile-find-test-file :which-key "find test file")
    )

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge
  :disabled)

(use-package diff-hl
  :hook
  (magit-pre-refresh-hook . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :ensure t
  :config (global-diff-hl-mode)
)

(defhydra hydra-git (:hint nil)
    "
Git Transient State
[_x_]: discard hunk [_n_]: next hunk [_p_/_N_]: previous hunk "
    ("n" diff-hl-next-hunk)
    ("N" diff-hl-previous-hunk)
    ("p" diff-hl-previous-hunk)
    ("x" diff-hl-revert-hunk)
    ("q" nil "quit" :color blue))

  (nmkip/leader-keys
    "g." '(hydra-git/body :which-key "Git transient state")
    "gs" '(magit-status :which-key "Magit Status")
    "gd" '(magit-diff-working-tree :which-key "Magit Diff working tree")
    "gn" '(hydra-git/diff-hl-next-hunk :which-key "Next hunk")
    "gN" '(hydra-git/diff-hl-previous-hunk :which-key "Previous hunk")
    "gp" '(hydra-git/diff-hl-previous-hunk :which-key "Previous hunk")
    "gx" '(hydra-git/diff-hl-revert-hunk :which-key "Revert hunk")

    "gD" '(:ignore t "Diff")
    "gDs" '(magit-diff-staged :which-key "Magit Diff staged")
    "gDu" '(magit-diff-unstaged :which-key "Magit Diff unstaged")
    "gDw" '(magit-diff-working-tree :which-key "Magit Diff working tree"))

(use-package undo-tree
    :diminish undo-tree-mode
    :ensure t
    :config 
        (global-undo-tree-mode)
        (evil-set-undo-system 'undo-tree))

(use-package smartparens
  :hook 
    (prog-mode . smartparens-mode)
    (emacs-lisp-mode . smartparens-strict-mode)
    (clojure-mode . smartparens-strict-mode)
    (cider-repl-mode . smartparens-strict-mode)
  :config
    (sp-local-pair sp-lisp-modes "'" nil :actions nil)
    (show-smartparens-global-mode))

(defadvice evil-delete-backward-char-and-join
    (around evil-delete-backward-char-and-join activate)
    (if (and (bound-and-true-p smartparens-mode)
            (bound-and-true-p smartparens-strict-mode))
        (call-interactively 'sp-backward-delete-char)
        ad-do-it))

(use-package evil-cleverparens
  :hook
      (clojure-mode . evil-cleverparens-mode)
      (emacs-lisp-mode . evil-cleverparens-mode)
      (cider-repl-mode . evil-cleverparens-mode)
  :custom
      (evil-cleverparens-use-additional-bindings nil)
      (evil-cleverparens-use-additional-movement-keys nil)
  :general
      (:keymaps '(normal visual)
                "M-[" 'evil-cp-beginning-of-defun
                "M-]" 'evil-cp-end-of-defun))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(recentf-mode)

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-h") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim" "clj")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))
