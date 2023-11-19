;;; config/default-exts/+macos.el -*- lexical-binding: t; -*-

(setq browse-url-mailto-function (lambda (link &rest _)
                                   (start-process "open" nil "open" link)))

;; Go away vile font panel
(defalias #'menu-set-font #'ignore)
(global-unset-key (kbd "s-t"))

;; Remove some more keybindings I don't need
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-q"))
(global-unset-key (kbd "s-w"))
