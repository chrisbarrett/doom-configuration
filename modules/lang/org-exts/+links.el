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
  :icon (all-the-icons-alltheicon "git" :height 0.9 :v-adjust 0.05)
  :follow (+ol-links-make-browse "github:" "https://github.com/%s")
  :format
  (lambda (url)
    (when (equal "github.com" (url-domain url))
      (concat "github:" (string-remove-prefix "/" (url-filename url))))))

(+declare-custom-org-link-type rust
  :icon (concat (all-the-icons-alltheicon "rust" :v-adjust 0.05))
  :follow
  (lambda (link &rest _)
    (browse-url (+crate-links-title-to-url link)))
  :format
  (lambda (url)
    (when (member  (url-domain url) ("doc.rust-lang.org" "docs.rs"))
      (let ((title (+crate-links-url-to-title (url-recreate-url url))))
        (concat "rust:" title)))))

;; deprecated
(+declare-custom-org-link-type rust-docs
  :icon (concat (all-the-icons-alltheicon "rust" :v-adjust 0.05))
  :follow
  (lambda (link &rest _)
    (browse-url (+crate-links-title-to-url link)))
  :format (-const nil))

;; deprecated
(+declare-custom-org-link-type docs.rs
  :icon (concat (all-the-icons-alltheicon "rust" :v-adjust 0.05))
  :follow
  (lambda (link &rest _)
    (browse-url (+crate-links-title-to-url link)))
  :format (-const nil))

(+declare-custom-org-link-type stackoverflow
  :icon (all-the-icons-faicon "stack-overflow" :height 0.9 :v-adjust 0.05)
  :follow (+ol-links-make-browse "stackoverflow:" "https://stackoverflow.com/%s")
  :format (lambda (url)
            (when (equal "stackoverflow.com" (url-domain url))
              (let ((title (+ol-guess-or-retrieve-title url 'no-elide)))
                (org-link-make-string (concat "stackoverflow:" (url-filename url))
                                      (string-remove-suffix " - Stack Overflow" title))))))

(+declare-custom-org-link-type slack
  :icon (all-the-icons-faicon "slack" :height 0.9 :v-adjust 0.05)
  :follow
  (lambda (link &rest _)
    (-let*  (((subdomain . path) (split-string (string-remove-prefix "slack:" link)
                                               "/"))
             (prefix (format "https://%s.slack.com" subdomain)))
      (browse-url (string-join (cons prefix path) "/"))))
  :format (lambda (url)
            (when (string-match-p (rx ".slack.com" eos) (url-domain url))
              (-let* (((subdomain) (split-string (url-domain url) "."))
                      (url-str (url-recreate-url url)))
                (org-link-make-string (concat "slack:")
                                      (read-string "Link Description: "))))))
