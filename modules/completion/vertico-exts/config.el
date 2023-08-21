;;; completion/vertico-exts/config.el -*- lexical-binding: t; -*-

(map! :after vertico
      :map vertico-map
      "C-k" #'kill-line
      "C-<return>" 'vertico-exit-input
      "M-<return>" 'minibuffer-force-complete-and-exit)
