;;; lang/org-exts/autoload/+roam.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'subr-x))

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
                               (disallowed (flatten-list (list '("daily")
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

;;;###autoload
(defun +roam-node-title-hierarchy (node)
  (thread-last
    (append (list (org-roam-node-file-title node))
            (org-roam-node-olp node)
            (list (org-roam-node-title node)))
    (seq-filter #'stringp)
    (seq-mapcat (fn! (split-string % ":")))
    (seq-map #'string-trim)
    (seq-uniq)))

(+describe +roam-node-title-hierarchy

  (+describe "node represents the file itself"
    (+it "returns document title"
      (let ((node (org-roam-node-create :file-title "file-title")))
        (should (equal (+roam-node-title-hierarchy node)
                       '("file-title"))))))

  (+describe "node is a root-level heading"
    (+it "returns file title joined with node title"
      (let ((node (org-roam-node-create :file-title "file-title" :title "title")))
        (should (equal (+roam-node-title-hierarchy node)
                       '("file-title" "title"))))))

  (+describe "node is nested under another heading"
    (+it "returns file title, olp and title joined"
      (let ((node (org-roam-node-create :file-title "file-title" :olp '("parent") :title "title")))
        (should (equal (+roam-node-title-hierarchy node)
                       '("file-title" "parent" "title"))))))

  (+describe "topic and title post-processing"

    (+describe "node contains duplicated parts"
      (+it "removes duplicates"
        (let ((node (org-roam-node-create :file-title "a" :olp '("a" "b" "a") :title "title")))
          (should (equal (+roam-node-title-hierarchy node)
                         '("a" "b" "title"))))))

    (+describe "colon-delimited prefixes in any titles"
      (+it "splits those parts"
        (let ((node (org-roam-node-create :file-title "a: file-title" :olp '("b" "c: d: e") :title "f: title")))
          (should (equal (+roam-node-title-hierarchy node)
                         '("a" "file-title" "b" "c" "d" "e" "f" "title"))))))))
