;; -*- lexical-binding: t; -*-

(after! (:and rustic-babel ob)
  (setq rustic-babel-default-toolchain "nightly"))

(use-package! eglot
  :hook ((rust-mode rustic-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode rust-mode) .
                                        ("rust-analyzer" :initializationOptions (:checkOnSave (:command "clippy"))))))
