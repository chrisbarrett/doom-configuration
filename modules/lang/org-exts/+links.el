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

(+declare-custom-org-link-type docs.rs
  :icon (concat (all-the-icons-alltheicon "rust" :v-adjust 0.05))
  :follow
  (lambda (link &rest _)
    (-let* (((crate-ref rest) (string-split link ":"))
            ((crate . version) (split-string crate-ref "@"))
            (version (or (car version) "latest"))
            (url-components (list "https://docs.rs" crate version crate rest)))
      (browse-url (string-join (seq-keep #'identity url-components)
                               "/"))))
  :format
  (lambda (url)
    (when (equal "docs.rs" (url-domain url))
      (let* ((parts
              (->> (url-filename url)
                   (string-remove-suffix "index.html")
                   (file-name-split)
                   (seq-drop-while #'seq-empty-p)
                   (seq-take-while (-not #'seq-empty-p))))
             (updated
              (pcase parts
                ((and `(,package ,version ,entry . ,rest)
                      (guard (equal package entry)))
                 (if (equal version "latest")
                     (concat package ":" (string-join rest "/"))
                   (concat package "@" version ":" (string-join (cons entry rest) "/"))))
                (_
                 (string-join parts "/")))))
        (concat "docs.rs:" updated)))))

(+declare-custom-org-link-type rust-docs
  :icon (concat (all-the-icons-alltheicon "rust" :v-adjust 0.05))
  :prefix "rust-docs:"
  :follow (+ol-links-make-browse "Rust Docs:" "https://doc.rust-lang.org/%s")
  :format
  (lambda (url)
    (when (equal "doc.rust-lang.org" (url-host url))
      (let* ((title (+ol-guess-or-retrieve-title url)))
        (org-link-make-string (concat "rust-docs:" (string-remove-prefix "/" (url-filename url)))
                              title)))))

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
