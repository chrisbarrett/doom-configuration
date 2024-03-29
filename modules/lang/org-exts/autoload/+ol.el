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

       ;; (+ol-simplified-title-for-url "https://doc.rust-lang.org/std/ops/trait.Fn.html")
       (extract "docs.rust-lang.org"
                (rx bol "https://doc.rust-lang.org/" (group (+? nonl)) (? ".html") eos)
                'no-host)

       (-some->> (extract "docs.rs"
                          (rx bol "https://docs.rs/" (group (+? nonl)) (? ".html") eos)
                          'no-host)
         (+crate-links-docs.rs-title))

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
                ((string-suffix-p "| MDN" title)
                 (concat (cadr (s-match (rx (group (+? nonl)) (? " - " (+? nonl)) " | MDN" eol)
                                        title))
                         " (MDN)"))
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

;;;###autoload
(defun +ol-title-for-url (url &optional edit)
  (let* ((parsed (url-generic-parse-url url))
         (functions (seq-keep #'cdr +ol-custom-format-functions-alist))
         (retrieved
          (or (-some->> (seq-reduce (lambda (acc fn)
                                      (if acc
                                          acc
                                        (funcall fn parsed)))
                                    functions
                                    nil)
                (s-match (rx "[[" (+? nonl) "][" (group (+? nonl)) "]]") )
                (cadr)
                (org-link-decode))
              (+ol-guess-or-retrieve-title url))))
    (if (or edit (null retrieved))
        (read-string "Title: " retrieved)
      retrieved)))

(defun +ol-format-as-some-link (url)
  (org-link-make-string url (+ol-title-for-url url)))

;;;###autoload
(defun +ol-insert-link (url)
  "Insert an orgmode link at point for URL."
  (interactive (list (+read-url)))
  (save-match-data
    (unless (thing-at-point-looking-at (rx bol (* space)))
      (just-one-space))
    (insert (+ol-format-as-some-link url))
    (just-one-space)))



(defun +ol--apply-custom-icon (start end icon)
  (let ((icon-string (apply 'propertize icon `(face (:height 0.8 :inherit success)
                                               ,@(text-properties-at 0 icon))))
        (space-string (propertize " " 'face '(:height 0.5)))
        (link (buffer-substring start end)))

    ;; TODO: Prevent icon from eating into protocol for bare links.

    (add-text-properties start
                         (+ start (1+ (s-index-of ":" link)))
                         (list 'display (concat icon-string space-string)))))

;;;###autoload
(cl-defun +declare-custom-org-link-type (type
                                         &key
                                         icon
                                         (format nil)
                                         (follow nil)
                                         (complete nil))
  (declare (indent 1))
  (after! org
    (let ((args (list
                 :activate-func (lambda (start end _path _bracketed-p)
                                  (+ol--apply-custom-icon start end icon)))))
      (when complete
        (appendq! args (list :complete complete)))
      (when follow
        (appendq! args (list :follow follow)))

      (apply #'org-link-set-parameters (symbol-name type) args)

      (setf (alist-get type +ol-custom-format-functions-alist) format))))

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
