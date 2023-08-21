;;; completion/defaults/config.el -*- lexical-binding: t; -*-

;; Hide commands irrelevant to current mode from M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)

;; Perform both indentation & text completion with TAB.
(setq tab-always-indent 'complete)

;; Emacs by default ignores a ton of stuff, but most of the entries don't matter
;; to me.
(setq completion-ignored-extensions
      '(
        ;; VC
        ".git/"
        ;; OS-generated
        ".DS_Store"
        ".o" "~" ".bin" ".lbin" ".so" ".a"
        ;; Emacs generated
        ".elc" ".eln"
        ;; Nix, direnv, etc
        ".drv"
        ".direnv/"
        ))
