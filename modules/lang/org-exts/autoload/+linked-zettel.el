;;; lang/org-exts/autoload/+linked-zettel.el -*- lexical-binding: t; -*-

(require 'org)
(require 'org-roam-node)
(require 'thingatpt)

(defun +linked-zettel--end-of-keyword-lines ()
  "Find the end position of a consecutive span of keywords, possibly
separated by blank lines."
  (save-excursion
    (let (stop end)
      (while (not (or stop (eobp)))
        (cond
         ;; If we're at a keyword line, we know the keyword span continues to at
         ;; least the end of the current line.
         ((org-at-keyword-p)
          (setq end (line-end-position))
          (forward-line 1))
         ;; Keep searching forward, but don't count trailing lines as part of the
         ;; span.
         ((string-blank-p (org-current-line-string))
          (forward-line 1))
         ;; We've hit something else, ending the search.
         (t
          (setq stop t))))
      end)))


(defun +linked-zettel--narrow-to-node-keywords (node)
  (widen)
  (goto-char (org-roam-node-point node))
  (cl-assert (or (bobp) (org-at-heading-p)))
  (when (org-at-heading-p)
    (forward-line 1)
    (let ((end (save-excursion (org-next-visible-heading 1) (point))))
      (narrow-to-region (point) end)))

  (when (thing-at-point-looking-at (rx bol ":PROPERTIES:"))
    (search-forward-regexp (rx bol ":END:"))
    (forward-line 1)
    (back-to-indentation))

  (narrow-to-region (point)
                    (or (+linked-zettel--end-of-keyword-lines)
                        (point-max))))


(defun +linked-zettel--goto-create-links-keyword (node)
  (cl-assert (equal (buffer-file-name) (org-roam-node-file node)))
  (save-restriction
    (+linked-zettel--narrow-to-node-keywords node)
    (unless (search-forward-regexp (rx bol "#+links:" (group (+ space) (* nonl))) nil t)
      (goto-char (or (+linked-zettel--end-of-keyword-lines)
                     (point-max)))
      (unless (string-blank-p (org-current-line-string))
        (newline))
      (insert "#+links: "))))



(defun +linked-zettel--links-at-point ()
  (save-match-data
    (cl-assert (thing-at-point-looking-at (rx bol "#+links:" (group (* nonl)))))
    (string-split (match-string-no-properties 1) (rx (any "|")) t (rx space))))

(defun +linked-zettel--set-links-at-point (links)
  (save-match-data
    (cl-assert (thing-at-point-looking-at (rx bol "#+links:" (group (* nonl)))))
    (goto-char (match-beginning 1))
    (delete-region (point) (line-end-position))
    (insert (concat " " (string-join links " | ")))))



;;;###autoload
(defun +linked-zettel-add (from-node to-node)
  "Add a link to another node to this one.

The link to TO-NODE is added as a #+link keyword for FROM-NODE."
  (interactive (list
                (org-roam-node-at-point t)
                (org-roam-node-read nil nil nil t "Add linked node: ")))

  (save-excursion
    (org-roam-with-file (org-roam-node-file from-node) nil
      (save-restriction
        (widen)

        (atomic-change-group
          (+linked-zettel--goto-create-links-keyword from-node)

          (let* ((new-link
                  (org-link-make-string (concat "id:" (org-roam-node-id to-node))
                                        (org-roam-node-formatted to-node)))
                 (updated
                  (append (+linked-zettel--links-at-point) (list new-link))))
            (+linked-zettel--set-links-at-point updated))))

      (message "Linked node added"))))
