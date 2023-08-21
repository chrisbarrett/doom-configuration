;; -*- lexical-binding: t; -*-

(after! eglot
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t))

(map! :map eglot-mode-map
      :after eglot
      "C-c C-r" 'eglot-rename
      "M-RET" 'eglot-code-actions)

(add-hook! eglot-managed-mode
           (eglot-inlay-hints-mode -1))
