;;; config/default-exts/+macos.el -*- lexical-binding: t; -*-

(setq browse-url-mailto-function (lambda (link &rest _)
                                   (start-process "open" nil "open" link)))

;; Go away vile font panel
(defalias #'menu-set-font #'ignore)
(global-unset-key (kbd "s-t"))
