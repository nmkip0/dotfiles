;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; Path to this file
(defvar emacs-org-config-path "~/.dotfiles/Emacs.org")

;; You will most likely need to adjust this font size for your system!
(defvar nmkip/default-font-size 120)
(defvar nmkip/fixed-pitch-font-size 120)
(defvar nmkip/variable-pitch-font-size 150)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t)

(setq display-line-numbers-type 'visual) ; Relative line numbers

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

    (nmkip/leader-keys
      "b"  '(:ignore t :which-key "buffers")
      "f"  '(:ignore t :which-key "files")
      "g"  '(:ignore t :which-key "git")
      "h"  '(:ignore t :which-key "help")
      "w"  '(:ignore t :which-key "windows")

      "SPC" '(counsel-M-x :which-key "M-x")
      "TAB" '(evil-switch-to-windows-last-buffer :which-key "Last buffer")
      "/" '(swiper :which-key "swiper")

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
      "bn" '(next-buffer :which-key "Next buffer")
      "bN" '(previous-buffer :which-key "Previous buffer")
      "bp" '(previous-buffer :which-key "Previous buffer")
      "bb" '(counsel-switch-buffer :which-key "Switch buffer")
      "bd" '(kill-this-buffer :which-key "Delete buffer")
      "bD" '(kill-buffer-and-window :which-key "Delete buffer and window")
      "bm" '(nmkip/show-messages-buffer :which-key "Messages buffer")
      "bs" '(nmkip/show-scratch-buffer :which-key "Scratch buffer")
      "br" '(Revert buffer :which-key "Revert buffer")

      ;; Files
      "fs" '(save-buffer :which-key "Save file")
      "fS" '(evil-write-all :which-key "Save all files")
      "fr" '(counsel-recentf :which-key "Recent files")
      "fR" '(rename-file :which-key "Rename file")

      ;; git
      ;; TODO: Add transient state: move to next/prev hunk. 
      "gs" '(magit-status :which-key "Next buffer")

      ;; help
      "ha" '(counsel-apropos :which-key "Apropos")
      "hb" '(counsel-describe-binds :which-key "Describe binds")
      "hf" '(counsel-describe-function :which-key "Describe function")
      "hF" '(counsel-describe-face :which-key "Describe face")
      "hk" '(describe-key :which-key "Describe key")
      "hm" '(describe-mode :which-key "Describe mode")
      "hp" '(describe-package :which-key "Describe package")
      "hs" '(counsel-symbol :which-key "Describe symbol")
      "ht" '(describe-theme :which-key "Describe theme")
      "hv" '(counsel-describe-variable :which-key "Describe variable")

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

      ))

(defhydra hydra-buffers (:hint nil)
    "
Buffer Transient State
[_n_]: next buffer [_p_/_N_]: previous buffer "
    ("n" next-buffer)
    ("N" previous-buffer)
    ("p" previous-buffer)
    ("q" nil "quit" :color blue))

(nmkip/leader-keys
    "b." '(hydra-buffers/body :which-key "Buffer transient state")
    "b," '(hydra-buffers/previous-buffer :which-key "Buffer transient state")
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

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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

(use-package doom-themes
  :init (load-theme 'doom-vibrant t))

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
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
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
  ;;     ("M-x" . counsel-M-x)
  ;;     ("C-x b" . counsel-switch-buffer)
  ;;     ("C-x C-f" . counsel-find-file)
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

(use-package cider
  :mode "\\.clj[sc]?\\'"
  :config
  (evil-collection-cider-setup))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
