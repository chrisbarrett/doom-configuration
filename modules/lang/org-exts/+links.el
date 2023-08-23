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

(after! ol
  (org-link-set-parameters
   "github"
   :activate-func (+link--prepend-custom-icon "" "github:")
   :follow (lambda (link &rest _)
             (browse-url (format "https://github.com/%s" (string-remove-prefix "github:" link)))))

  (dolist (type '("http" "https"))
    (org-link-set-parameters type :activate-func (+link--prepend-custom-icon "↗"))))
