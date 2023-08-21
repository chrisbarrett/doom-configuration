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
