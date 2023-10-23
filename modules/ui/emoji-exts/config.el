;;; ui/emoji-exts/config.el -*- lexical-binding: t; -*-

(after! emojify
  (setq emojify-download-emojis-p t)
  (setq emojify-point-entered-behaviour 'uncover)
  (setq emojify-display-style 'unicode)
  (delq! 'string emojify-program-contexts)

  (add-hook! 'emojify-mode-hook
             ;; Remove checkbox unicode emoji that conflicts with org-superstar.
             (remhash "☑" emojify-emojis))

  (add-to-list 'emojify-inhibit-major-modes 'flymake-diagnostics-buffer-mode)
  (add-to-list 'emojify-inhibit-functions #'+emoji-at-org-drawer-p))
