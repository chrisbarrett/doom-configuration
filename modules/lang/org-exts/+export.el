;;; lang/org-exts/+export.el -*- lexical-binding: t; -*-

(setq org-export-backends '(ascii html latex odt slack gfm))
(setq org-export-with-toc nil)
(setq org-html-html5-fancy t)
(setq org-html-postamble nil)
(setq org-export-exclude-tags '("noexport" "ignore" "crypt"))
(setq org-export-coding-system 'utf-8)
