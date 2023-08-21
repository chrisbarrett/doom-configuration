;;; lang/emacs-lisp-exts/config.el -*- lexical-binding: t; -*-

(map! "C-c e e" #'toggle-debug-on-error)

(after! elisp-mode
  (map! :map emacs-lisp-mode-map
        "C-c C-c" #'eval-defun
        "C-c C-m" #'pp-macroexpand-last-sexp
        [remap +format/region-or-buffer] #'+elisp-indent-dwim)

  (defface +emacs-triple-semi-comment-text
    '((t
       (:inherit magit-section-secondary-heading :italic nil)))
    "Face for the content of ;;; comments."
    :group 'lang-emacs-lisp)

  (font-lock-add-keywords 'emacs-lisp-mode
                          `((,(rx bol (* space) ";;;" (* ";") (+ space) (group (* nonl)))
                             1 '+emacs-triple-semi-comment-text prepend))))

(after! ielm
  (map! :map ielm-map :ni "C-k" #'sp-kill-hybrid-sexp))

;; Use configured load-path if we're in a doom config file.

(setq-hook! emacs-lisp-mode
  elisp-flymake-byte-compile-load-path (+elisp-flymake-byte-compile-load-path-compute))

;; Fall back to `elisp-slime-nav-find-elisp-thing-at-point' when looking up definitions.

(set-lookup-handlers! '(emacs-lisp-mode lisp-interaction-mode helpful-mode)
  :documentation '(elisp-slime-nav-describe-elisp-thing-at-point)
  :definition '(+emacs-lisp-lookup-definition elisp-slime-nav-find-elisp-thing-at-point))
