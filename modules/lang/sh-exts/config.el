;;; lang/sh-exts/config.el -*- lexical-binding: t; -*-

;;; Shell-scripting

(add-to-list 'auto-mode-alist (cons (rx "sshd_config" eol) 'conf-unix-mode))
(add-to-list 'auto-mode-alist (cons (rx "/sshd_config.d/") 'conf-unix-mode))

;;; Env files

(add-to-list 'auto-mode-alist (cons (rx "/.env" (? "." (+ nonl)) eol) 'conf-unix-mode))
(add-to-list 'auto-mode-alist (cons (rx "/.envrc") 'bash-ts-mode))


;;; Oils for Unix
;;
;; I eagerly await a real major mode, if this scripting language takes off.

(add-to-list 'auto-mode-alist (cons (rx ".oil" eos) 'prog-mode))
(add-to-list 'magic-mode-alist (cons (rx bol "#!/usr/bin/env" (+ space) "oil" symbol-end) 'prog-mode))
