;;; ox-slack.el --- Export backend for Slack-style markdown.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ox)
(require 'ox-gfm)
(require 'subr-x)

(autoload 'gfm-mode "markdown-mode")

(defgroup ox-slack nil
  "Export backend for Slack markup."
  :group 'languages
  :prefix "ox-slack-")

(defcustom ox-slack-postprocess-function #'identity
  "Function for transforming the output of the exported org tree.

Useful for replacing references to people with username, etc."
  :type 'function)

(defun ox-slack--markup-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (todo (org-export-data (org-element-property :todo-keyword headline) info))
         (todo-text (unless (or (not (plist-get info :with-todo-keywords))
                                (string= todo ""))
                      todo)))
    (concat
     (when todo-text (concat todo-text " "))
     "*" text "*"
     "\n\n" (when (org-string-nw-p contents) contents))))

(defun ox-slack--passthrough (_ contents _info)
  (format "%s" contents))

(defun ox-slack--italic (_italic contents _info)
  (format "_%s_" contents))

(defun ox-slack--bold (_bold contents _info)
  (format "*%s*" contents))

(defun ox-slack--strike-through (_italic contents _info)
  (format "~%s~" contents))

(defun ox-slack--item (item contents info)
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "-"
                   (concat (number-to-string
                            (car (last (org-list-get-item-number
                                        (org-element-property :begin item)
                                        struct
                                        (org-list-prevs-alist struct)
                                        (org-list-parents-alist struct)))))
                           "."))))
    (concat bullet
            " "
            (pcase (org-element-property :checkbox item)
              (`on "`[X]` ")
              (`trans "`[-]` ")
              (`off "`[ ]` "))
            (let ((tag (org-element-property :tag item)))
              (and tag (format "*%s:* "(org-export-data tag info))))
            (and contents
                 (org-trim (replace-regexp-in-string "^" "    " contents))))))

(defun ox-slack--link (&rest args)
  (pcase-let* ((`((link ,link-attrs) ,_ ,_) args)
               (link-type (plist-get link-attrs :type)))
    (apply (if (equal "id" link-type)
               #'ox-slack--passthrough
             #'org-md-link)
           args)))

(defun ox-slack--fixed-width-block (example-block _contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "```\n%s\n```"
          (org-remove-indentation
           (org-export-format-code-default example-block info))))

(defun ox-slack--timestamp (timestamp _contents _info)
  (org-timestamp-translate timestamp))

(defmacro ox-slack--with-default-export-options (&rest body)
  (declare (indent 0))
  `(let ((org-export-with-author nil)
         (org-export-with-toc nil)
         (org-export-with-creator nil)
         (org-html-special-string-regexps nil)
         (org-export-with-email nil))
     ,@body))

;;;###autoload
(defun ox-slack-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (ox-slack--with-default-export-options
    (org-export-to-buffer 'slack "*Org Slack Export*"
      async subtreep visible-only body-only ext-plist
      (lambda ()
        (let ((str (funcall ox-slack-postprocess-function (buffer-string))))
          (erase-buffer)
          (insert str)
          (gfm-mode))))))

;;;###autoload
(defun ox-slack-export-to-clipboard (&optional async subtreep visible-only body-only ext-plist formatter)
  (interactive)
  (let ((org-export-show-temporary-export-buffer nil))
    (ox-slack--with-default-export-options
      (org-export-to-buffer 'slack "*Org Slack Export*"
        async subtreep visible-only body-only ext-plist
        (lambda ()
          (let* ((str (funcall ox-slack-postprocess-function (string-trim (buffer-string))))
                 (postprocessed (funcall (or formatter #'identity) str)))
            (kill-new postprocessed))
          (message "Buffer contents copied to clipboard"))))))



(defcustom ox-slack-name-translation-alist nil
  "Alist of regular expressions to Slack handles."
  :group 'ox-slack
  :type '(alist string string))

;;;###autoload
(defun ox-slack-translate-names (str)
  (seq-reduce
   (-lambda (acc (name . handle))
     (replace-regexp-in-string (rx-to-string `(and bow (regexp ,name) eow)
                                             t)
                               handle
                               acc t))
   ox-slack-name-translation-alist
   str))

;;;###autoload
(defun ox-slack-copy ()
  "Export the subtree at point as Slack markdown."
  (interactive)
  (cl-assert (org-at-heading-p))
  (let ((heading (org-get-heading)))
    (ox-slack-export-to-clipboard nil
                                  t
                                  nil
                                  nil
                                  nil
                                  (lambda (str)
                                    (format "*%s*\n\n%s" (string-trim heading) str)))))

(provide 'ox-slack)

;;; ox-slack.el ends here
