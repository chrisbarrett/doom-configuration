;;; checkers/syntax-exts/config.el -*- lexical-binding: t; -*-

(map! :map 'flymake-mode-map
      "M-n" #'flymake-goto-next-error
      "M-p" #'flymake-goto-prev-error)
