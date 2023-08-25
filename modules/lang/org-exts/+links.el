;;; lang/org-exts/+links.el -*- lexical-binding: t; -*-

(setq org-link-elisp-confirm-function 'y-or-n-p)
(setq org-return-follows-link t)

(after! evil
  (define-advice org-return (:around (fn &rest args) inhibit-follow-in-insert-state)
    (let ((org-return-follows-link (if (evil-insert-state-p)
                                       nil
                                     org-return-follows-link)))
      (apply fn args))))

(after! ol
  ;; Open links in the current window
  (setf (alist-get 'file org-link-frame-setup) 'find-file))

;;; Define link types with pretty icons

(+declare-custom-org-link-type http
  :icon "↗"
  :face-properties (:v-adjust 0.05))

(+declare-custom-org-link-type https
  :icon "↗"
  :face-properties (:v-adjust 0.05))

(+declare-custom-org-link-type man
  :icon (all-the-icons-faicon "book" :height 0.9 :v-adjust -0.05)
  :follow (lambda (query &rest _)
            (man query)))

(+declare-custom-org-link-type github
  :icon (all-the-icons-alltheicon "git")
  :follow (lambda (link &rest _)
            (let ((path (string-remove-prefix "github:" link)))
              (browse-url (format "https://github.com/%s" path)))))
