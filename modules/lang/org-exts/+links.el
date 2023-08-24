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

;;; Add indicators to fontified URL links

(+declare-custom-org-link-type http :icon "↗")
(+declare-custom-org-link-type https :icon "↗")

(+declare-custom-org-link-type github
  :icon (all-the-icons-alltheicon "git")
  :face-properties (:height 1)
  :follow (lambda (link &rest _)
            (let ((path (string-remove-prefix "github:" link)))
              (browse-url (format "https://github.com/%s" path)))))
