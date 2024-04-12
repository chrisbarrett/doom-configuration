;;; completion/defaults/config.el -*- lexical-binding: t; -*-

;; Hide commands irrelevant to current mode from M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)

;; Perform both indentation & text completion with TAB.
(setq tab-always-indent 'complete)

(dolist (entry '(".DS_Store" ".eln" ".drv" ".direnv/" ".git/"))
  (add-to-list 'completion-ignored-extensions entry))
