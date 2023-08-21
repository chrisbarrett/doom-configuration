;;; lang/org-exts/autoload/+tex.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-at-latex-fragment-p ()
  (let ((datum (org-element-context)))
    (and (memq (org-element-type datum) '(latex-environment latex-fragment)))))

;;;###autoload
(defun +org-at-latex-preview-p ()
  (seq-find
   (lambda (ov)
     (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
   (overlays-at (point))))
