;; Local leader
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(define-key evil-motion-state-map (kbd "] e") #'flycheck-next-error)
(define-key evil-motion-state-map (kbd "[ e") #'flycheck-previous-error)

;; Vim keys
(general-define-key :states 'normal "\\" 'evil-snipe-repeat-reverse)

(map! :map (clojure-mode-map clojurescript-mode-map emacs-lisp-mode-map)
       :localleader

       :prefix ("s" . "Structural editing")

       :desc "Raise sexp"
       "r" #'paredit-raise-sexp

       :desc "Splice sexp"
       "x" #'paredit-splice-sexp

       :desc "Forward barf"
       "b" #'paredit-forward-barf-sexp

       :desc "Forward slurp"
       "s" #'paredit-forward-slurp-sexp

       :desc "Backward barf"
       "B" #'paredit-backward-barf-sexp

       :desc "Backward slurp"
       "S" #'paredit-backward-slurp-sexp)

(map! :after cider-mode
      :map cider-mode-map
      :localleader
      :prefix "e"
      :desc "Cider eval last sexpr and copy to clipboard"
      "c" #'cider-eval-last-sexpr-and-copy-to-clipboard
      "l" #'nmkip/cider-eval-sexp-end-of-line
      "v" #'cider-eval-sexp-at-point)

(map! :localleader
      :map (clojure-mode-map clojurescript-mode-map)
      :n "!" #'k16.dev.system/reset)

(map! :localleader
      :map (clojure-mode-map clojurescript-mode-map)
      :prefix "P"
      :n "o" #'portal.api/open
      :n "l" #'portal.api/clear
      :n "q" #'portal.api/close)

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
      ">" #'nmkip/hydra-window/evil-window-increase-width)

(defhydra nmkip/hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "scale up")
  ("j" text-scale-decrease "scale down")
  ("0" doom/reset-font-size "reset font")
  ("q" nil "quit" :exit t))

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

(map! :leader
      (:prefix "t"
       :desc "Global Command Log"
       "L" #'global-command-log-mode)
      (:prefix "b"
       :desc "Toggle Command Log buffer"
       "L" #'clm/toggle-command-log-buffer))

(after! company
  (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(after! clojure-mode
   (defhydra hydra-portal (clojure-mode-map "C-k")
     "Portal"
     ("9" portal.ui.commands/toggle-selection "Toggle selection")
     ("8" portal.ui.commands/cycle-viewer "Set tree viewer")
     ("1" portal.ui.commands/copy "Copy")
     ("2" portal.ui.commands/copy-path "Copy path")
     ("3" portal.ui.commands/copy-json "Copy json")
     ("r" portal.ui.commands/select-root "Select root")
     ("e" portal.ui.commands/toggle-expand "Toggle expand")
     ("j" portal.ui.commands/select-next "Select next")
     ("k" portal.ui.commands/select-prev "Select prev")
     ("h" portal.ui.commands/select-parent "Select parent")
     ("l" portal.ui.commands/select-child "Select child")
     ("C-h" portal.ui.commands/history-back "History back")
     ("C-l" portal.ui.commands/history-forward "History forward")
     ("RET" portal.ui.commands/focus-selected "Focus selected")
     ("C-;" portal.api/clear "Clear" :exit t)
     ("q" nil "Exit" :exit t)))

(map! :leader
      :desc "portal"
      "k" #'hydra-portal/body)

;; nested hydra working mehh
;; (defhydra hydra-portal-copy ()
;;     "Copy"
;;     ("y" portal.ui.commands/copy "Copy")
;;     ("j" portal.ui.commands/copy-json "Copy JSON")
;;     ("p" portal.ui.commands/copy-path "Copy Path")
;;     ("b" hydra-portal/body "Back")
;;     ("q" nil "Exit"))

;; (defhydra hydra-portal ()
;;     "Portal"
;;     ("r" portal.ui.commands/select-root "Select root")
;;     ("e" portal.ui.commands/toggle-expand "Toggle expand")
;;     ("j" portal.ui.commands/select-next "Select next")
;;     ("k" portal.ui.commands/select-prev "Select prev")
;;     ("h" portal.ui.commands/select-parent "Select parent")
;;     ("l" portal.ui.commands/select-child "Select child")
;;     ("y" hydra-portal-copy/body "Copy" :exit t)
;;     ("C-h" portal.ui.commands/history-back "History back")
;;     ("C-l" portal.ui.commands/history-forward "History forward")
;;     ("RET" portal.ui.commands/focus-selected "Focus selected")
;;     ("C-;" portal.api/clear "Clear" :exit t)
;;     ("q" nil "Exit" :exit t))
