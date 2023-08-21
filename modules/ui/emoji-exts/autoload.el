;;; ui/emoji-exts/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emoji-at-org-drawer-p (&rest _)
  (when (derived-mode-p 'org-mode 'org-agenda-mode)
    (save-excursion
      (goto-char (line-beginning-position))
      (or (org-at-drawer-p) (org-at-property-p)))))
