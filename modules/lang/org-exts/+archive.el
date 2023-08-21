;;; lang/org-exts/+archive.el -*- lexical-binding: t; -*-

(setq org-archive-tag "archived")
(setq org-archive-location (concat (expand-file-name "archive.org" org-directory) "::datetree/"))
(setq org-archive-subtree-add-inherited-tags nil)

;; Automatically save after archiving.
(add-hook 'org-archive-hook #'org-save-all-org-buffers)
