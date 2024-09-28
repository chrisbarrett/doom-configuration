;;; ui/doom-exts/+macos.el -*- lexical-binding: t; -*-

(modify-all-frames-parameters '((undecorated . t)))

(defun +macos-sdk-header-file-locations ()
  (let ((sdk-root
         (string-trim-right (shell-command-to-string "xcrun --show-sdk-path"))))
    (expand-file-name "usr/include" sdk-root)))

(after! man
  (pushnew! Man-header-file-path (+macos-sdk-header-file-locations)))
