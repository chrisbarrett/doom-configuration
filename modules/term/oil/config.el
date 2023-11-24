;;; term/oil/config.el -*- lexical-binding: t; -*-


;; I eagerly await a real major mode, if this scripting language takes off.

(add-to-list 'auto-mode-alist (cons (rx ".oil" eos) 'prog-mode))
(add-to-list 'magic-mode-alist (cons (rx bol "#!/usr/bin/env" (+ space) "oil" symbol-end) 'prog-mode))
