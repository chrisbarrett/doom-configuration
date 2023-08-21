;; -*- lexical-binding: t; -*-

(after! sh-script
  (setq sh-basic-offset 2))

(add-to-list 'auto-mode-alist (cons (rx "/.envrc") 'bash-ts-mode))
