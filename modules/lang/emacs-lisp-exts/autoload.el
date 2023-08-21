;;; lang/emacs-lisp-exts/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +elisp-flymake-byte-compile-load-path-compute ()
  (cons "./"
        (when (locate-dominating-file default-directory
                                      (fn! (or (file-equal-p doom-user-dir %)
                                               (file-equal-p org-directory %))))
          load-path)))

;;;###autoload
(defun +elisp-indent-dwim ()
  "Perform a context-sensitive indentation action."
  (interactive)
  (if (region-active-p)
      (indent-region (region-beginning) (region-end))
    (let ((message-log-max))
      (indent-region (point-min) (point-max))
      (message "Buffer indented"))))
