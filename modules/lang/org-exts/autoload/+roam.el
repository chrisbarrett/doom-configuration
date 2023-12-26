;;; lang/org-exts/autoload/+roam.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +roam-node-find (&optional other-window)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix arg OTHER-WINDOW, visit the node in another
window."
  (interactive "P")
  (org-roam-node-find other-window
                      nil
                      (lambda (node)
                        (let* ((tags (org-roam-node-tags node))
                               (disallowed (flatten-list (list '("daily" "litnotes")
                                                               (when (and timekeep-mode (org-clocking-p))
                                                                 "private")))))
                          (null (seq-intersection tags disallowed))))))

;;;###autoload
(defun +roam-follow-link-other-window ()
  "Force the link at point to open in another window."
  (interactive)
  (let ((org-link-frame-setup (cons '(file .
                                      (lambda ()
                                        (when-let* ((buf (find-file-noselect filename)))
                                          (display-buffer-in-direction buf '((direction . rightmost))))))
                                    org-link-frame-setup)))
    (org-open-at-point)))

;;;###autoload
(defun +roam-backlinks-dedicated (node)
  "Display a dedicated backlinks buffer for NODE."
  (interactive (list (if current-prefix-arg
                         (org-roam-node-read nil nil nil 'require-match)
                       (org-roam-node-at-point 'assert))))
  (org-roam-buffer-display-dedicated node))

(defvar +roam--file-to-node-title-lookup (make-hash-table :test 'equal)
  "Dynamic variable used for caching in `org-roam-node-find'.")

;;;###autoload
(defun +roam-node-file-cache-rebuild ()
  (clrhash +roam--file-to-node-title-lookup)
  (pcase-dolist (`(,file ,title)
                 (org-roam-db-query [:select [file title]
                                     :from nodes
                                     :where (= 0 level)]))
    (puthash file title +roam--file-to-node-title-lookup)))

(defun +roam-node-title-for-file (file)
  (when (hash-table-empty-p +roam--file-to-node-title-lookup)
    (+roam-node-file-cache-rebuild)
    (run-with-idle-timer 1 nil 'garbage-collect))
  (gethash file +roam--file-to-node-title-lookup))

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
