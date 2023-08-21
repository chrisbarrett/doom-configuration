;;; config/default-exts/+macos.el -*- lexical-binding: t; -*-

(setq browse-url-mailto-function (lambda (link &rest _)
                                   (start-process "open" nil "open" link)))
