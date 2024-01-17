;;; lang/org-exts/autoload/+roam-default-headings.el -*- lexical-binding: t; -*-

(defgroup +roam-default-headings nil
  "Create a default set of headings for all nodes."
  :group 'productivity
  :prefix "+roam-default-headings-")

(defcustom +roam-default-headings-function #'+roam-default-headings-for-node
  "A function that takes a node and returns a list of default heading names.

The function should return a list, where each item is a heading
in the order it should appear in the file.

Each item in the list may be either:

1. A heading name as a string

2. A plist, with the following keys:

   - :name (required) - the name of the heading

   - :ensure - whether the heading should be created if it does
     not exist. Default is true.

   - :dblock - a dynamic block that should be present under the
     heading. The value for this key should be a dblock header
     plist."
  :group '+roam-default-headings
  :type 'function)



(defun +roam-default-headings-for-node (_node)
  '(("Dates" :ensure nil)
    "Notes"
    "Topics"
    ("Tasks" :ensure nil)
    "Related"
    ("References" :dblock (:name "backlinks" :tags litnotes :only-missing t))
    ("Planning & Meetings" :tags ("outline") :ensure nil)
    ("Footnotes" :ensure nil)))

(defmacro +roam-default-headings--save-excursion-via-text-properties (&rest body)
  "Like `save-excursion', but works using text properties.

Some destructive buffer modifications will defeat
`save-excursion'. This approach uses text properties instead,
which may still be preserved through structural edits to the
buffer.

Tries a couple of heuristics to put point at least somewhere
close to the starting point from before BODY was executed."
  (declare (indent 0))
  `(progn
     ;; Tidy up trailing whitespace to ensure we restore to a valid column after buffer transformations.
     (when (string-match-p (rx bol (* space) eol) (buffer-substring (point) (line-end-position)))
       (delete-horizontal-space))

     (let ((start (point))
           (col (current-column)))

       (org-with-wide-buffer
        (back-to-indentation)
        ;; Put down some text properties at various places to see if we can restore
        ;; any of them later.
        ;;
        ;; Applying these text properties will fail if the file is empty.
        (unless (zerop (buffer-size))
          (ignore-errors
            (add-text-properties (point) (1+ (point)) '(+roam-default-headings-sentinel point)))
          (ignore-errors
            (add-text-properties (point) (1+ (point)) '(+roam-default-headings-sentinel indentation)))
          (when (org-up-heading-safe)
            (ignore-errors
              (add-text-properties (point) (1+ (point)) '(+roam-default-headings-sentinel heading))))))

       (let ((result (progn ,@body)))

         (goto-char (point-min))
         (if-let* ((prop-match
                    (or
                     (save-excursion
                       (text-property-search-forward '+roam-default-headings-sentinel 'point #'equal))
                     (save-excursion
                       (text-property-search-forward '+roam-default-headings-sentinel 'indentation #'equal))
                     (save-excursion
                       (text-property-search-forward '+roam-default-headings-sentinel 'heading #'equal)))))
             (progn
               (goto-char (prop-match-beginning prop-match))
               (move-to-column col))
           ;; Being smart failed; go back to the absolute starting point.
           (goto-char start))

         (remove-text-properties (point-min) (point-max) '(+roam-default-headings-sentinel
                                                           ;; NB. actual value is ignored.
                                                           _))
         ;; Text property removal above breaks inline images. Redisplay them.
         (org-display-inline-images)

         result))))

(defun +roam-default-headings--sync-buffer (specs)
  (cl-labels ((ensure-dblock (props)
                (cl-assert (listp props))
                (cl-assert (org-at-heading-p))
                (save-restriction
                  (org-narrow-to-subtree)
                  (let ((name (plist-get props :name)))
                    (unless (search-forward-regexp (rx-to-string `(and bol (* space) "#+BEGIN:" (+ space) ,name)) nil t)
                      (goto-char (point-max))
                      (org-create-dblock props)))))

              (find-or-create-heading (title)
                (cl-assert (stringp title))
                (or (org-find-exact-headline-in-buffer title)
                    (progn
                      ;; Create title if it doesn't exist
                      (goto-char (point-max))
                      (unless (bolp) (newline))
                      (let (org-insert-heading-respect-content)
                        (org-insert-heading nil nil t))
                      (insert title)
                      (goto-char (line-beginning-position))
                      (point-marker)))))

    (-each specs
      (-lambda ((it &as name &plist :ensure :dblock :tags))
        (when-let* ((marker (if ensure
                                (find-or-create-heading name)
                              (org-find-exact-headline-in-buffer name))))
          (org-with-point-at marker
            (org-set-tags tags)
            (while (save-excursion (org-get-next-sibling))
              (org-move-subtree-down))
            (when dblock
              (ensure-dblock dblock)))
          (set-marker marker nil))))))

(defun +roam-default-headings--update-p (specs)
  (let* ((required (->> (seq-filter (lambda (it) (plist-get (cdr-safe it) :ensure)) specs)
                        (seq-map #'car)))
         (topmost
          (org-element-map (org-element-parse-buffer) '(data headline)
            (-lambda ((_ heading)) (plist-get heading :raw-value)) nil nil '(headline)))

         (required-present-p (null (seq-difference required topmost)))

         (all-managed-headings (seq-map #'car specs))
         (buffer-managed-headings (seq-intersection topmost all-managed-headings))
         (managed-headings-expected-order (seq-intersection all-managed-headings topmost)))

    (not (and required-present-p
              (equal buffer-managed-headings managed-headings-expected-order)))))

(defun +roam-default-headings--specs-for-node ()
  (when (org-roam-file-p)
    (when-let* ((node (save-excursion
                        (goto-char (point-min))
                        (org-roam-node-at-point))))
      (--map (append (-list it) (list :ensure t :dblock nil))
             (funcall +roam-default-headings-function node)))))

;;;###autoload
(defun +roam-default-headings-populate ()
  "Populate the current roam node with headings."
  (interactive)
  (when-let* ((specs (+roam-default-headings--specs-for-node)))
    (when (+roam-default-headings--update-p specs)
      (atomic-change-group
        (+roam-default-headings--save-excursion-via-text-properties
          (+roam-default-headings--sync-buffer specs)
          (org-format-buffer))))))

(provide '+roam-default-headings)

;;; +roam-default-headings.el ends here
