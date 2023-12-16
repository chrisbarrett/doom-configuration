;;; lang/org-exts/+nursery.el -*- lexical-binding: t; -*-

;;; org-roam-review

(map! :after org-roam
      :map org-roam-review-mode-map
      :n "/" #'org-roam-review-modify-tags
      :n "TAB" #'magit-section-cycle
      :n "g r" #'org-roam-review-refresh
      :n [remap evil-next-line] #'evil-next-visual-line
      :n [remap evil-previous-line] #'evil-previous-visual-line
      :n :desc "Accept" "C-c r r" #'org-roam-review-accept
      :n :desc "Forgot" "C-c r f" #'org-roam-review-forgot
      :n :desc "Bury" "C-c r u" #'org-roam-review-bury
      :n :desc "Set excluded" "C-c r x" #'org-roam-review-set-excluded
      :n :desc "Set memorise" "C-c r m" #'org-roam-review-set-memorise
      :n :desc "Set budding" "C-c r b" #'org-roam-review-set-budding
      :n :desc "Set seedling" "C-c r s" #'org-roam-review-set-seedling
      :n :desc "Set evergreen" "C-c r e" #'org-roam-review-set-evergreen)

(map! :after org-roam
      :map org-mode-map
      :desc "Accept" "C-c r r" #'org-roam-review-accept
      :desc "Forgot" "C-c r f" #'org-roam-review-forgot
      :desc "Bury" "C-c r u" #'org-roam-review-bury
      :desc "Set excluded" "C-c r x" #'org-roam-review-set-excluded
      :desc "Set memorise" "C-c r m" #'org-roam-review-set-memorise
      :desc "Set budding" "C-c r b" #'org-roam-review-set-budding
      :desc "Set seedling" "C-c r s" #'org-roam-review-set-seedling
      :desc "Set evergreen" "C-c r e" #'org-roam-review-set-evergreen)

(after! org-roam
  (require 'org-roam-review))

(after! org-roam-review
  (add-hook 'org-roam-review-next-node-selected-hook 'org-roam-buffer--redisplay-h 91)

  (setq org-roam-review-ignored-tags '("daily" "litnotes"))
  (setq org-roam-review-tags-ignored-for-review-buffer '("daily" "litnotes"))
  (setq org-roam-review-show-instructions-p nil))

;;; org-roam-rewrite

(after! org-roam-rewrite
  (setq org-roam-rewrite-rename-without-confirmation-p t)
  (setq org-roam-rewrite-extract-excluded-tags
        (seq-uniq (append
                   org-roam-rewrite-extract-excluded-tags
                   org-roam-review-maturity-values
                   ;; Note types
                   '("notes" "moc" "outline" "litnotes"))))


  (when (modulep! :tools magit)
    ;; Remove deleted nodes from Git automatically.

    (autoload 'magit-call-git "magit-process")
    (autoload 'magit-convert-filename-for-git "magit-git")

    (add-hook 'org-roam-rewrite-node-removed-functions
              (-lambda ((&plist :file :level))
                (let ((default-directory (file-name-directory file)))
                  (when (zerop level)
                    (magit-call-git "rm" (magit-convert-filename-for-git file)))))))


  ;; Ignore possibly-missing topic prefix when checking which backlinks were
  ;; customised

  (defun +org-roam-node-normalised-title (node-or-title)
    (let* ((title-str (if (stringp node-or-title)
                          node-or-title
                        (org-roam-node-title node-or-title)))
           (last-part (-last-item (string-split title-str ":" t (rx space)))))
      (replace-regexp-in-string (rx (+ (any space "\n"))) "" (downcase last-part))))

  (setq org-roam-rewrite-backlink-transformer
        (-lambda ((&plist :prev-node :new-id :new-desc :prev-desc))
          (let* ((norm-titles (cons (+org-roam-node-normalised-title prev-node)
                                    (seq-map #'+org-roam-node-normalised-title
                                             (org-roam-node-aliases prev-node))))
                 (desc-customised-p
                  (not (seq-contains-p norm-titles (+org-roam-node-normalised-title prev-desc)))))
            (list :id new-id :desc (if desc-customised-p prev-desc new-desc))))))

(defun +org-roam-ad-format-buffer (fn &rest args)
  (let* ((result (apply fn args))
         (buf (if (bufferp result) result (current-buffer))))
    (with-current-buffer buf
      (+roam-default-headings-populate)
      (let ((save-silently t)
            (message-log-max))
        (save-buffer)))
    buf))

(advice-add 'org-roam-node-visit :around #'+org-roam-ad-format-buffer)
(advice-add 'org-roam-preview-visit :around #'+org-roam-ad-format-buffer)
(advice-add 'org-link-open :around #'+org-roam-ad-format-buffer)

(add-hook 'org-mode-hook #'org-format-on-save-mode)

;;; org-roam-search

(autoload 'org-roam-search "org-roam-search")

(map! :map org-roam-mode-map
      :after org-roam
      :nm "s" #'org-roam-search)

;; Compute previews lazily

(after! org-roam
  (require 'org-roam-lazy-previews))

;; Buffer showing backwards & forward links

(autoload 'org-roam-links "org-roam-links")

(define-advice org-roam-node-open (:after (&rest _) update-links)
  (when (get-buffer-window "*org-roam-links*")
    (save-window-excursion
      (org-roam-links))))

;;; dblocks

(after! org-roam
  (require 'org-roam-dblocks))

;; Don't enable for template files.
(add-hook! 'org-mode-hook
  (unless (or (string-match-p (rx ".template.org" eos) (buffer-name))
              (derived-mode-p 'snippet-mode))
    (org-roam-dblocks-autoupdate-mode +1)))

(define-advice org-roam-id-open (:around (fn &rest args) refresh-dblocks-on-id-open)
  (let* ((id (car args))
         (node (org-roam-node-from-id id))
         (existing-buf (-some->> node (org-roam-node-file) (find-buffer-visiting))))
    (prog1 (apply fn args)
      (when existing-buf
        (unless (buffer-modified-p existing-buf)
          (with-current-buffer existing-buf
            (org-update-all-dblocks)))))))
