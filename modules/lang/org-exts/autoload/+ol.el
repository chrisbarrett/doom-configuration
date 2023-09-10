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

       ;; (+ol-simplified-title-for-url "https://github.com/org/repo/pulls/33")
       (extract "GitHub"
                (rx-to-string `(and bol "https://github.com/"
                                    (group (+? nonl) (or "/issues/" "/pull/") (+ digit))
                                    ,query eol))
                t)

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
                    (group (+ nonl)))
                t)

       ;; (+ol-simplified-title-for-url "https://github.com/org/repo")
       (extract "GitHub" (rx bol "https://github.com/" (group (+ nonl)))
                t)

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
(defun +ol-guess-or-retrieve-title (url &optional no-elide)
  (require 'org-cliplink)
  (let* ((url (if (stringp url) url (url-recreate-url url)))
         (title
          (or (+ol-simplified-title-for-url url)
              (+ol--postprocess-retrieved-title url
                                                (let ((org-cliplink-max-length 1024))
                                                  (org-cliplink-retrieve-title-synchronously url))))))
    (if no-elide
        title
      (org-cliplink-elide-string title org-cliplink-max-length))))

(defvar +ol-custom-format-functions-alist nil
  "Alist of link TYPE to format function.")

(defun +ol-format-as-some-link (url)
  (let ((parsed (url-generic-parse-url url))
        (functions (seq-keep #'cdr +ol-custom-format-functions-alist)))
    (or (seq-reduce (lambda (acc fn)
                      (if acc
                          acc
                        (funcall fn parsed)))
                    functions nil)
        (org-link-make-string url
                              (or (+ol-guess-or-retrieve-title url) (read-string "Title: "))))))

;;;###autoload
(defun +ol-insert-link (url)
  "Insert an orgmode link at point for URL."
  (interactive (list (+read-url)))
  (save-match-data
    (unless (thing-at-point-looking-at (rx bol (* space)))
      (just-one-space))
    (insert (+ol-format-as-some-link url))
    (just-one-space)))



(defun +ol--apply-custom-icon (start icon &optional prefix face-properties)
  (when prefix
    (add-text-properties start (+ start (length prefix)) '(invisible t)))
  (add-text-properties
   start (1+ start)
   (list 'display
         (concat
          (propertize icon
                      'face
                      `(,@(text-properties-at 0 icon)
                        ,@face-properties
                        :height 0.8
                        :inherit success))
          (propertize " " 'face '(:height 0.5))))))

;;;###autoload
(cl-defmacro +declare-custom-org-link-type (type
                                            &key
                                            icon
                                            (format nil)
                                            (prefix nil)
                                            (follow nil)
                                            (face-properties nil)
                                            (complete nil))
  (declare (indent 1))
  (let* ((name (symbol-name type))
         (prefix (or (eval prefix) (format "%s:" name))))
    `(progn
       (org-link-set-parameters
        ,name
        :activate-func (lambda (start &rest _)
                         (+ol--apply-custom-icon start ,icon ,prefix ',face-properties))
        ,@(when complete (list :complete complete))
        ,@(when follow (list :follow follow)))

       (setf (alist-get ,name +ol-custom-format-functions-alist) ,format))))

;;;###autoload
(defun +ol-links-make-browse (link-prefix domain)
  (lambda (link &rest _)
    (let ((path (string-remove-prefix link-prefix link)))
      (browse-url (format domain path)))))

;;;###autoload
(defun +ol-links-make-format (domain link-prefix)
  (lambda (url)
    (when (equal domain (url-domain url))
      (concat link-prefix (string-remove-prefix "/" (url-filename url))))))
