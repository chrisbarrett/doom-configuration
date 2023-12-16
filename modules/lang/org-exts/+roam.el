;;; lang/org-exts/+roam.el -*- lexical-binding: t; -*-

(map! :after org-roam
      :map org-mode-map
      :ni :desc "Insert org-roam link" "C-c i" #'org-roam-node-insert
      :ni "C-c TAB" (general-predicate-dispatch #'org-roam-node-insert
                      (org-at-table-p) 'org-ctrl-c-tab)
      :ni "S-<return>" (general-predicate-dispatch #'+roam-follow-link-other-window
                         (org-at-table-p) 'org-table-copy-down)

      ;; Prevent C-c C-TAB from toggling table column on roam link insertion
      ;;
      ;; Emacs thinks C-c C-TAB and C-c C-i are the same key code. In org
      ;; tables, this means the keybinding will collapse the current column
      ;; instead of inserting a link.
      ;;
      ;; Change this behaviour so that a roam link is inserted if a region is
      ;; active while in a table.
      :v "C-c C-I" (general-predicate-dispatch 'org-roam-node-insert
                     (and (org-at-table-p) (not (region-active-p))) 'org-ctrl-c-tab))

(map! :localleader :map org-mode-map
      :after org-roam
      :desc "Backlinks" "<tab>"
      (cmd! (when-let* ((buf (get-buffer org-roam-buffer)))
              (with-current-buffer buf
                (org-roam-buffer-refresh)))
            (org-roam-buffer-toggle))
      :desc "Backlinks (dedicated)" "<backtab>" #'+roam-backlinks-dedicated

      :desc "Extract subtree" "E" #'org-roam-rewrite-extract
      :desc "Rename" "r" #'org-roam-rewrite-rename
      :desc "Inline" "I" #'org-roam-rewrite-inline
      :desc "Delete & redirect" "D" #'org-roam-rewrite-remove

      (:prefix-map ("l" . "alias")
       :desc "add alias" "a" #'org-roam-alias-add
       :desc "remove alias" "x" #'org-roam-alias-remove)

      (:prefix-map ("k" . "tags")
       :desc "add tag" "a" #'org-roam-tag-add
       :desc "remove tag" "x" #'org-roam-tag-remove))

(after! org-roam
  (setq org-roam-verbose nil)
  (setq org-roam-extract-new-file-path "notes/%<%Y-%m-%d--%H-%M-%S>.org")
  (setq org-roam-mode-sections '((org-roam-backlinks-section :unique t)
                                 (org-roam-reflinks-section)))

  ;; Keep the DB in sync.
  (org-roam-db-autosync-enable))

(use-package! org-roam-gc
  :after org-roam
  :demand t
  :hook (org-mode . org-roam-gc-automatically))

;; Apply a CREATED timestamp property to new nodes
(add-hook! 'org-roam-capture-new-node-hook
  (org-with-wide-buffer
   (unless (org-entry-get (point) "CREATED")
     (org-set-property "CREATED" (format-time-string (org-time-stamp-format t t))))))

(when (modulep! +everywhere)
  ;; Allow ID look-ups to roam nodes from non-roam org files.

  (add-hook! 'org-mode-hook
    (add-hook! 'after-save-hook :local t
      (when-let* ((id (org-entry-get-with-inheritance "ID"))
                  (file (buffer-file-name)))
        (org-id-add-location id file))))

  (pcase-dolist (`(,id ,file)
                 (org-roam-db-query "SELECT id, file FROM nodes"))
    (org-id-add-location id file)))

;;; Index with slipbox as tag

(after! org-roam
  (org-roam-slipbox-buffer-identification-mode +1)
  (org-roam-slipbox-tag-mode +1))

;;; Formatting

(add-hook! 'org-mode-hook
  (add-hook 'before-save-hook #'+roam-default-headings-populate nil t))

(add-hook! (org-roam-rewrite-node-renamed
            org-roam-rewrite-node-extracted)
  (run-with-idle-timer 3 nil #'+roam-node-file-cache-rebuild))

(after! org-roam
  (eval `(progn

           (cl-defmethod org-roam-node-formatted-title ((node org-roam-node))
             (pcase-let ((`(,title . ,rest) (nreverse (+roam-node-title-hierarchy node))))
               (let ((prefix (seq-map (fn! (propertize % 'face 'org-property-value)) (nreverse rest)))
                     (title (propertize title 'face 'org-roam-title)))

                 (string-join (append prefix (list title))
                              (propertize ": " 'face 'org-property-value)))))

           (cl-defmethod org-roam-node-icon ((node org-roam-node))
             (require 'org-roam-review)
             (condition-case nil
                 (when-let* ((maturity (car (seq-intersection org-roam-review-maturity-values (org-roam-node-tags node)))))
                   (alist-get maturity org-roam-review-maturity-emoji-alist nil nil #'string=))
               (error "")))))

  (setq org-roam-node-formatter #'org-roam-node-formatted-title)
  (setq org-roam-review-title-formatter #'org-roam-node-formatted-title)

  ;; Customise completion UI
  (setq org-roam-node-display-template
        (concat
         "${formatted-title:*} "
         " ${icon:3} "
         (propertize "@${slipbox:9}" 'face 'org-tag)
         "${tags:*}")))
