;; -*- lexical-binding: t; -*-

(after! browse-at-remote

  ;; KLUDGE: Override Doom's advice so that `main' is used as the default branch
  ;; rather than `master'.

  (defadvice! +vc--fallback-to-master-branch-a ()
    "Return 'main' in detached state."
    :after-until #'browse-at-remote--get-local-branch
    "main")

  ;; KLUDGE: fix issue preventing resolution of remote branch due to missing origin.

  (define-advice browse-at-remote--get-remote-branch (:override (local-branch) fix-missing-remote)
    (cons (car (browse-at-remote--get-remotes)) local-branch)))
