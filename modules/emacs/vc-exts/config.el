;; -*- lexical-binding: t; -*-

(after! browse-at-remote

  ;; KLUDGE: Override Doom's advice so that `main' is used as the default branch
  ;; rather than `master'.

  (defadvice! +vc--fallback-to-master-branch-a ()
    "Return 'main' in detached state."
    :after-until #'browse-at-remote--get-local-branch
    "main"))
