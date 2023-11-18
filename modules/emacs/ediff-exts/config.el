;; -*- lexical-binding: t; -*-

(after! ediff
  (advice-add 'ediff-next-difference :after #'+ediff-reveal-org-content-around-hunk)
  (advice-add 'ediff-previous-difference :after #'+ediff-reveal-org-content-around-hunk)

  (setq ediff-show-clashes-only t)

  (add-hook! 'ediff-keymap-setup-hook
             ;; Keymap is buffer-local
             (map! :map ediff-mode-map "B" #'+ediff-copy-both-to-C)))
