;;; lang/org-exts/autoload/+links.el -*- lexical-binding: t; -*-

(autoload 'org-cliplink-elide-string "org-cliplink-string")

(defun +ol--simplify-url (regexp url)
  (-some->> (s-match regexp url)
    (cdr) ;; elt 0 is the whole string
    (seq-map (fn! (string-replace "+" " " %)))
    (s-join "/…/")))

;;;###autoload
(defun +ol-simplified-title-for-url (url)
  (cl-labels ((extract (host regexp &optional exclude-host-p)
                (when-let* ((simplified (+ol--simplify-url regexp url)))
                  (if exclude-host-p
                      simplified
                    (format "%s (%s)" simplified host)))))
    (let ((query '(? "?" (* nonl))))
      (or
       (extract "Confluence"
                (rx ".atlassian.net/wiki/spaces/" (+? nonl) "/pages/" (+? nonl) "/" (group (+ nonl))))
       (extract "Jira"
                (rx-to-string `(and ".atlassian.net/browse/" (group (+? nonl)) ,query eol)))

       (extract "GitHub"
                (rx-to-string `(and bol "https://github.com/"
                                    (group (+? nonl) (or "/issues/" "/pull/") (+ digit))
                                    ,query eol)))

       ;; (+ol-simplified-title-for-url "https://linear.app/COMPANY/issue/KEY-0000/the-quick-brown-fox")
       (-some->> (extract "Linear"
                          (rx-to-string `(and bol "https://linear.app/"
                                              (+ alnum) "/issue/" (+? nonl) "/" (group (+? nonl)) ,query eol))
                          'no-host)
         (string-replace "-" " "))

       ;; (+ol-simplified-title-for-url "https://trello.com/c/00000000/1000-the-quick-brown-fox")
       (-some->> (extract "Trello"
                          (rx-to-string `(and bol "https://trello.com/c/"
                                              (+ alnum) "/" (+ digit) "-" (group (+? nonl)) ,query eol)))
         (string-replace "-" " "))

       ;; (+ol-simplified-title-for-url "https://app.shortcut.com/companydomain/story/10000/the-quick-brown-fox")
       (-some->> (extract "Shortcut"
                          (rx-to-string `(and bol "https://app.shortcut.com/"
                                              (+? any) "/" (+? any) "/" (+ digit) "/" (group (+? nonl)) ,query eol)))
         (string-replace "-" " "))

       ;; (+ol-simplified-title-for-url "https://github.com/org/repo/blob/master/path/file.md")
       (extract "GitHub"
                (rx bol "https://github.com/"
                    (group (+? nonl) "/" (+ nonl))
                    "/blob/" (+? nonl) "/"
                    (group (+ nonl))))


       ;; (+ol-simplified-title-for-url "https://github.com/org/repo")
       (extract "GitHub" (rx bol "https://github.com/" (group (+ nonl))))

       (when (string-match-p (rx bol "https://" (+? any) ".slack.com/") url)
         "Slack link")))))

(defun +ol--postprocess-retrieved-title (url title)
  (string-trim (cond
                ((string-match-p (rx "investopedia.com") url)
                 (concat title " (Investopedia)"))
                ((string-suffix-p "| Microsoft Docs" title)
                 (string-remove-suffix "| Microsoft Docs" title))
                ((string-prefix-p "https://developer.apple.com/library/archive/documentation/" url)
                 (concat title " (Apple Developer Archive)"))
                (t
                 title))))

(defvar org-cliplink-max-length)

;;;###autoload
(defun +ol-guess-or-retrieve-title (url)
  (let ((title
         (or (+ol-simplified-title-for-url url)
             (+ol--postprocess-retrieved-title url
                                               (let ((org-cliplink-max-length 1024))
                                                 (org-cliplink-retrieve-title-synchronously url))))))
    (org-cliplink-elide-string title org-cliplink-max-length)))

;;;###autoload
(defun +ol-insert-link (url)
  "Insert an orgmode link at point for URL."
  (interactive (list (+read-url)))
  (save-match-data
    (let ((title (or (+ol-guess-or-retrieve-title url)
                     (read-string "Title: "))))
      (unless (thing-at-point-looking-at (rx bol (* space)))
        (just-one-space))
      (insert (org-link-make-string url title))
      (just-one-space))))
