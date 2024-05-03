;; -*- lexical-binding: t; -*-

(after! ediff
  (advice-add 'ediff-next-difference :after #'+ediff-reveal-org-content-around-hunk)
  (advice-add 'ediff-previous-difference :after #'+ediff-reveal-org-content-around-hunk)

  (setq-default ediff-show-clashes-only t))
