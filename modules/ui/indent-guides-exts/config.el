;;; ui/indent-guides-exts/config.el -*- lexical-binding: t; -*-

(remove-hook! (prog-mode text-mode conf-mode) 'highlight-indent-guides-mode)
(add-hook! (yaml-mode yaml-ts-mode nxml-mode python-ts-mode) 'highlight-indent-guides-mode)

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-responsive t))
