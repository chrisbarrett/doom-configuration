;;; lang/org-exts/+src.el -*- lexical-binding: t; -*-

(setq org-src-preserve-indentation nil)
(setq org-src-fontify-natively t)
(setq org-src-window-setup 'current-window)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-shell-names '("sh" "bash" "zsh" "oil"))

(setq! org-babel-load-languages '((emacs-lisp . t)
                                  (C . t)
                                  (clojure . t)
                                  (csharp . t)
                                  (dot . t)
                                  (gnuplot . t)
                                  (sql . t)
                                  (python . t)
                                  (calc . t)
                                  (shell . t)))

(setq org-babel-default-header-args:emacs-lisp '((:lexical . "yes")))
(setq org-babel-clojure-backend 'cider)
(setq org-babel-python-command "python3")

;; Show images in outputs (useful for GNUplot, etc).
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images t)

;; Prevent trailing whitespace from being created in src blocks.
(define-advice org-edit-src-exit (:before (&rest _) delete-ws)
  (delete-trailing-whitespace))

(setq-hook! org-src-mode-hook require-final-newline nil)
