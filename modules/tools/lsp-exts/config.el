;; -*- lexical-binding: t; -*-

(map! (:map eglot-mode-map
       :after eglot
       "C-c C-r" 'eglot-rename
       "M-RET" 'eglot-code-actions)

      (:map 'flymake-mode-map
            "M-n" #'flymake-goto-next-error
            "M-p" #'flymake-goto-prev-error))

(after! eglot
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t)
  (eglot-booster-mode +1))

(add-hook! eglot-managed-mode
  (eglot-inlay-hints-mode -1))

;;; Java - woe is me

(after! eglot
  (setf (alist-get '(java-mode java-ts-mode) eglot-server-programs nil nil #'equal)
        (cons "jdtls"
              (seq-map (lambda (s) (concat "--jvm-arg=" s))
                       (list "-XX:+UseParallelGC"
                             "-XX:GCTimeRatio=4"
                             "-XX:AdaptiveSizePolicyWeight=90"
                             "-Dsun.zip.disableMemoryMapping=true"
                             "-Dlog.protocol=true"
                             "-Dlog.level=ALL"
                             "-Xmx1G"
                             "-Xms100m"
                             (concat "-javaagent:" (getenv "NIX_EMACS_LOMBOK_JAR")))))))


;;; Rust

(after! (:and rustic-babel ob)
  (setq rustic-babel-default-toolchain "nightly"))

(after! eglot
  (setf (alist-get '(rust-mode rust-ts-mode) eglot-server-programs nil nil #'equal)
        '("rust-analyzer" :initializationOptions (:checkOnSave (:command "clippy")))))
