;;; lang/org-exts/config.el -*- lexical-binding: t; -*-

(defvar +roam-index-node-id nil
  "ID for the node to use as entrypoint to org-roam.")

(add-hook 'org-mode-hook #'abbrev-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)

(when (modulep! +modern)
  (add-hook 'org-mode-hook #'org-modern-mode)
  (after! 'org
    (setq org-modern-list nil)
    (setq org-auto-align-tags nil)
    (setq org-tags-column nil)
    (setq org-agenda-tags-column 0)
    (setq org-modern-star '("*"))
    (setq org-modern-todo-faces '(("WAIT" warning :bold t :inverse-video t)))))

(add-hook! 'org-mode-hook
  (when (locate-dominating-file default-directory
                                (fn! (file-equal-p org-directory %)))
    (setq-local abbrev-file-name (expand-file-name "abbrev.el" org-directory))))

(add-hook 'org-mode-hook #'org-appear-mode)

(map! "C-c a" 'org-agenda)

(map! :after org
      :map org-mode-map
      :ni "C-c l" #'+ol-insert-link
      :ni "C-c f" 'org-footnote-new
      :ni "C-c C-k" (general-predicate-dispatch 'org-cut-subtree
                      (bound-and-true-p org-capture-mode) 'org-capture-kill
                      (string-prefix-p "*Org" (buffer-name)) 'org-kill-note-or-show-branches)
      :ni "C-c RET" (general-predicate-dispatch 'org-insert-todo-heading
                      (org-at-table-p) 'org-table-hline-and-move)
      :i "<tab>" (general-predicate-dispatch 'org-cycle
                   (and (modulep! :editor snippets) (yas--templates-for-key-at-point)) #'yas-expand)

      :n "<backtab>" 'org-global-cycle
      :n "<tab>" 'org-cycle
      :n "C-c c" 'org-columns
      :n "C-c d" 'org-dynamic-block-insert-dblock
      :n "C-c n" 'org-next-link
      :n "C-c p" 'org-previous-link
      :n "M-n" 'org-metadown
      :n "M-p" 'org-metaup
      :n "RET" 'org-open-at-point
      :n "t"   'org-todo

      :ni "M-+" 'org-table-insert-column
      :ni "M--" 'org-table-delete-column
      :ni "C-c C-." 'org-time-stamp-inactive
      :ni "C-c ." 'org-time-stamp
      :ni "C-c o" 'org-table-toggle-coordinate-overlays)

(map! :after org :localleader :map org-mode-map
      :desc "Copy subtree" "y" #'org-copy-subtree
      :desc "Cut subtree" "x" #'org-cut-subtree
      :desc "Paste tree" "p" #'org-paste-subtree
      :desc "Todo tree" "t" #'org-show-todo-tree)

(when (modulep! +nursery)
  (map! "<f12>" (general-predicate-dispatch 'timekeep-start
                  (and (fboundp 'org-clocking-p) (org-clocking-p)) 'timekeep-stop)))

(after! org
;;; Visual settings

  (setq org-cycle-separator-lines 0)
  (setq org-ellipsis " …")
  (setq org-hide-emphasis-markers t)
  (setq org-indent-mode-turns-on-hiding-stars t)
  (setq org-pretty-entities t)
  (setq org-startup-folded 'showall)
  (setq org-startup-indented t)
  (setq org-startup-shrink-all-tables t)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview nil)

  ;; Don't show secondary selection when running `org-show-todo-tree'.
  (advice-add #'org-highlight-new-match :override #'ignore)

;;; TODOs, checkboxes, stats, properties.

  (setq org-todo-keywords '((type "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c@)")))
  (setq org-checkbox-hierarchical-statistics t)
  (setq org-checkbox-hierarchical-statistics t)
  (setq org-enforce-todo-dependencies t)
  (setq org-hierarchical-todo-statistics nil)
  (setq org-use-property-inheritance t)

  ;; Completing all child TODOs will change the parent TODO to DONE.
  (add-hook! 'org-after-todo-statistics-hook
    (fn! (let (org-log-done) ; turn off logging
           (org-todo (if (zerop %2) "DONE" "TODO")))))

;;; Interactive behaviour

  (setq org-bookmark-names-plist nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-adapt-indentation nil)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (setq org-fold-catch-invisible-edits 'smart)
  (setq org-footnote-auto-adjust t)
  (setq org-insert-heading-respect-content t)
  (setq org-loop-over-headlines-in-active-region 'start-level)

  (when IS-MAC
    ;; Ensure we use dired rather than the Finder on macOS.
    (add-to-list 'org-file-apps '(directory . emacs))))

(when (and (modulep! +initial-buffers)
           (modulep! +roam))
  (autoload 'org-roam-review-buffers "org-roam-review")
  (autoload 'org-roam-node-visit "org-roam")

  (defun +org-roam-initial-buffers ()
    (cl-assert +roam-index-node-id)
    (let ((inhibit-redisplay t)
          (buf))
      (org-roam-node-visit (org-roam-node-from-id +roam-index-node-id))
      (setq buf (current-buffer))
      (goto-char (point-min))
      (delete-other-windows)
      (org-roam-review)
      (when-let* ((win (-some->> (org-roam-review-buffers) (car) (get-buffer-window))))
        (select-window win)
        (set-window-dedicated-p win t))
      (display-buffer buf)
      buf))

  (setq initial-buffer-choice #'+org-roam-initial-buffers))

;; KLUDGE: Doom is attempting to set bindings on this mode, but evil-org appears
;; to have removed it.
(after! org
  (defalias 'evil-org-agenda-mode 'ignore)
  (defvar evil-org-agenda-mode-map (make-sparse-keymap)))

;; Remove doom's default capture templates.
(remove-hook 'org-load-hook #'+org-init-capture-defaults-h)

(after! evil-org
  (setq evil-org-special-o/O '(table-row item)))

(after! evil
  (setq evil-org-key-theme '(todo navigation insert textobjects additional calendar)))

;; Automatically enter insert state when inserting new headings, logbook notes
;; or when using `org-capture'.

(after! evil
  (defadvice! +enter-evil-insert-state (&rest _)
    :after '(org-insert-heading
             org-insert-heading-respect-content
             org-insert-todo-heading-respect-content
             org-insert-todo-heading)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil))
      (evil-insert-state)))

  (define-advice org-capture (:after (&rest _) insert-state)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil)
               (bound-and-true-p org-capture-mode))
      (evil-insert-state)))

  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state))

(after! org-modern
  (setq org-modern-star (delete "✳" org-modern-star)))

(after! org
  (load! "+agenda")
  (load! "+archive")
  (load! "+attach")
  (load! "+clock")
  (load! "+export")
  (load! "+links")
  (load! "+src")

  (when (or (modulep! +roam)
            (modulep! :lang org +roam2))
    (load! "+roam"))

  (when (modulep! +nursery)
    (load! "+nursery")))

(when (modulep! +citations)
  (load! "+citations"))

(when (modulep! +slack)
  (load! "+slack"))
