;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +evil-ret ()
  (interactive)
  (or (ignore-errors
        (goto-address-at-point)
        t)
      (call-interactively #'evil-ret)))
