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
                               (disallowed (flatten-list (list '("dailies" "litnotes")
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
