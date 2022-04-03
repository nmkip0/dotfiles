(use-package! paredit
  :hook ((clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (clojurec-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)))

(after! paredit
  ;; Like https://github.com/tpope/vim-sexp-mappings-for-regular-people
  (general-define-key :states 'normal "<)" 'paredit-forward-barf-sexp)
  (general-define-key :states 'normal ">)" 'paredit-forward-slurp-sexp)
  (general-define-key :states 'normal "<(" 'paredit-backward-slurp-sexp)
  (general-define-key :states 'normal ">(" 'paredit-backward-barf-sexp))
