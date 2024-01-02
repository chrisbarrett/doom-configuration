;;; lang/org-exts/autoload/+linked-zettel.el -*- lexical-binding: t; -*-

(require 'org)
(require 'org-roam-node)
(require 's)
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
  "Attempt to narrow to any keywords at the start of NODE.

If a sequence of keyword lines is found at the start of NODE,
narrow to those lines and return the end position of those lines.

If no keyword lines are matched, leave buffer widened and return
nil."
  (widen)
  (goto-char (org-roam-node-point node))
  ;; If the node starts somewhere other than a heading or the file-level, we
  ;; have no idea how to proceed.
  (cl-assert (or (bobp) (org-at-heading-p)))

  ;; If we're at a heading, pre-narrow to:
  ;;
  ;; 1. exclude the heading, and
  ;;
  ;; 2. include only the remainder of content at that heading's level.

  (when (org-at-heading-p)
    (forward-line 1)
    (let ((end (save-excursion (org-next-visible-heading 1) (point))))
      (narrow-to-region (point) end)))

  ;; Skip properties drawer if present.
  (when (search-forward-regexp (rx bol ":PROPERTIES:") nil t)
    (search-forward-regexp (rx bol ":END:"))
    (forward-line 1)
    (back-to-indentation))

  (when (org-at-keyword-p)
    (let ((start (point))
          (end (+linked-zettel--end-of-keyword-lines)))
      (narrow-to-region start end)
      end)))


(defun +linked-zettel--goto-links-keyword (node &optional create)
  (cl-assert (equal (buffer-file-name) (org-roam-node-file node)))
  (save-restriction
    (let ((bound
           (or (+linked-zettel--narrow-to-node-keywords node)
               (save-excursion
                 (org-next-visible-heading 1)
                 ;; possibly EOB
                 (point)))))

      (if (search-forward-regexp (rx bol "#+links:") bound t)
          (progn
            (goto-char (line-beginning-position))
            (point))

        (when create
          (when-let* ((pos (+linked-zettel--end-of-keyword-lines)))
            (goto-char pos)
            (newline))

          (insert "#+links: ")
          (when (org-current-level)
            (save-excursion
              (newline 2)))
          (goto-char (line-beginning-position))
          (point))))))



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




(defun +linked-zettel--parse-link (str)
  (-let [(_ id desc)
         (s-match (rx bol "[[id:" (group (+? nonl)) "]"
                      ;; desc is optional in links
                      (? (and "[" (group (+? nonl)) "]"))
                      "]" eol)
                  str)]
    (list :id id :desc desc)))

(defun +linked-zettel--render-link (link-plist)
  (-let [(&plist :id :desc) link-plist]
    (org-link-make-string (concat "id:" id) desc)))

(defun +linked-zettel--modify-links (node fn)
  "Modify the links for NODE in-place.

NODE is the target node. FN is a callback used to update the list
of linked nodes. It is passed a plist with `:id' and `:desc'
keys, and should return a list of the same type."
  (save-excursion
    (org-roam-with-file (org-roam-node-file node) nil
      (save-buffer)
      (save-restriction
        (widen)
        (atomic-change-group
          (+linked-zettel--goto-links-keyword node t)
          (let* ((current (seq-map #'+linked-zettel--parse-link (+linked-zettel--links-at-point)))
                 (updated (funcall fn current)))
            (if (null updated)
                (delete-region (line-beginning-position) (1+ (line-end-position)))
              (+linked-zettel--set-links-at-point (seq-map '+linked-zettel--render-link updated)))))))))



(defun +linked-zettel--read-non-linked-node (node prompt)
  (let* ((this-node (org-roam-node-id node))
         (excluded-ids
          (cons this-node
                (save-excursion
                  (when (+linked-zettel--goto-links-keyword node)
                    (seq-map #'+linked-zettel--node-link-id
                             (+linked-zettel--links-at-point)))))))

    (org-roam-node-read nil
                        (lambda (it)
                          (not (member (org-roam-node-id it) excluded-ids)))
                        nil
                        t
                        prompt)))

;;;###autoload
(defun +linked-zettel-add (from-node to-node)
  "Add a link to another node to this one.

The link to TO-NODE is added as a #+link keyword for FROM-NODE."
  (interactive
   (let ((node (org-roam-node-at-point t)))
     (list node (+linked-zettel--read-non-linked-node node "Node to add: "))))
  (+linked-zettel--modify-links from-node
                                (lambda (links)
                                  (seq-uniq (append links `((:id ,(org-roam-node-id to-node)
                                                             :desc ,(org-roam-node-formatted to-node))))
                                            (-on #'equal (lambda (it) (plist-get it :id))))))
  (message "Linked node added"))



(defun +linked-zettel--node-link-id (s)
  (cadr (s-match (rx "[id:" (group (+? nonl)) "]") s)))


(defun +linked-zettel--read-node-from-links (node prompt)
  (save-excursion
    (+linked-zettel--goto-links-keyword node)
    (let ((linked-ids (seq-map #'+linked-zettel--node-link-id
                               (+linked-zettel--links-at-point))))
      (org-roam-node-read nil
                          (lambda (it)
                            (member (org-roam-node-id it) linked-ids))
                          nil
                          t
                          prompt))))

;;;###autoload
(defun +linked-zettel-remove (from-node to-node)
  "Remove an reference to a linked node.

The link to TO-NODE will be removed from the #+link keyword for
FROM-NODE."
  (interactive
   (let ((node (org-roam-node-at-point t)))
     (list node (+linked-zettel--read-node-from-links node "Node to remove: "))))

  (cl-assert to-node)
  (+linked-zettel--modify-links from-node
                                (lambda (links)
                                  (seq-remove (lambda (it)
                                                (equal (plist-get it :id)
                                                       (org-roam-node-id to-node)))
                                              links)))
  (message "Linked node removed"))
