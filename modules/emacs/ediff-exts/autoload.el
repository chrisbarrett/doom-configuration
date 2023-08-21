;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +ediff-copy-both-to-C ()
  "Copy both ediff buffers in a 3-way merge to the target buffer."
  (interactive)
  (let ((str
         (concat
          (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
          (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
    (ediff-copy-diff ediff-current-difference nil 'C nil str)))

;;;###autoload
(defun +ediff-reveal-org-content-around-hunk (&rest _)
  (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (when (derived-mode-p 'org-mode)
          (org-reveal t))))))
