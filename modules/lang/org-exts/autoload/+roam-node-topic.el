;;; lang/org-exts/autoload/+roam-node-topic.el -*- lexical-binding: t; -*-

;;; Commentary:

;; I sometimes like to scope notes to a particular topic, using a `TOPIC: NAME'
;; convention. This is different from having separate slipboxes, since I use
;; slipboxes as /types/ (evergreens, stubs & nouns vs lit notes).

;;; Code:

;;;###autoload
(defface +roam-node-topic nil
  "Face for node topics."
  :group 'org-roam)

;;;###autoload
(defun +roam-node-topic-parse (node-or-title)
  (pcase-let* ((original (if (stringp node-or-title) node-or-title (org-roam-node-title node-or-title)))
               (`(,_ ,subject ,title)
                (s-match (rx (* space) (group (+? nonl)) (* space) (any "/:") (+ space) (group (+ nonl)))
                         original)))
    (if subject
        (list :subject subject :title title)
      (list :title original))))

(defvar +roam-node--file-to-title-lookup (make-hash-table :test 'equal)
  "Dynamic variable used for caching in `org-roam-node-find'.")

;;;###autoload
(defun +roam-node-file-cache-rebuild ()
  (clrhash +roam-node--file-to-title-lookup)
  (pcase-dolist (`(,file ,title)
                 (org-roam-db-query [:select [file title]
                                     :from nodes
                                     :where (= 0 level)]))
    (puthash file title +roam-node--file-to-title-lookup)))

(defun +roam-node-title-for-file (file)
  (when (hash-table-empty-p +roam-node--file-to-title-lookup)
    (+roam-node-file-cache-rebuild)
    (run-with-idle-timer 1 'garbage-collect))
  (gethash file +roam-node--file-to-title-lookup))

;;;###autoload
(defun +roam-node-title-hierarchy (node)
  (if-let* ((file-title (+roam-node-title-for-file (org-roam-node-file node)))
            (path-components
             (seq-mapcat (fn! (string-split % ":" t (rx space)))
                         (cons file-title
                               (unless (zerop (org-roam-node-level node))
                                 `(,@(org-roam-node-olp node)
                                   ,(org-roam-node-title node)))))))
      (pcase path-components
        (`(,_)
         path-components)
        (`(,title . ,rest)
         (cons title (seq-map (fn! (string-trim (string-remove-prefix title %)))
                              rest))))

    ;; Fall back gracefully to normal title if looking up the title failed.
    (list
     (org-roam-node-title node))))
