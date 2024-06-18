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

(add-hook 'nix-mode-hook #'eglot-ensure)
(add-hook 'yaml-ts-mode-hook #'eglot-ensure)
(add-hook 'json-mode-hook #'eglot-ensure)
