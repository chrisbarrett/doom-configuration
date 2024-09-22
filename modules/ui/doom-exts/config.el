;;; ui/doom-exts/config.el -*- lexical-binding: t; -*-

(setq display-line-numbers-type 'relative)

(use-package! page-break-lines
  :hook (doom-first-input . global-page-break-lines-mode)
  :config
  (setq page-break-lines-modes '(prog-mode
                                 org-mode
                                 org-agenda-mode
                                 latex-mode
                                 help-mode
                                 special-mode)))

(map! "C-c SPC" 'emojify-insert-emoji
      "C-x SPC" 'insert-char
      :map (global-map) [remap make-frame] #'ignore)

(when IS-MAC
  (load! "+macos"))

;; Many Doom modules add rainbow-delimiters-mode to mode hooks. Rather than
;; trying to remove them all, just make calling the function a no-op.
(defalias 'rainbow-delimiters-mode #'ignore)

(use-package! paren-face
  :defines (paren-face-modes)
  :hook (doom-first-input . global-paren-face-mode)
  :config
  (setq paren-face-regexp (rx (any "{}();,")))

  (pushnew! paren-face-modes 'csharp-mode 'js-base-mode 'lisp-data-mode 'typescript-ts-base-mode 'yaml-ts-mode 'zig-mode)

  (font-lock-add-keywords 'js-base-mode `((,(rx (any ":")) 0 'parenthesis)))
  (font-lock-add-keywords 'typescript-ts-base-mode `((,(rx (any ":")) 0 'parenthesis)))
  (font-lock-add-keywords 'zig-mode `((,(rx (any ":")) 0 'parenthesis)))
  )

;; Prevent display-buffer from creating new frames

(defun +display-buffer-fallback (buffer &rest _)
  (when-let* ((win (split-window-sensibly)))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window))))
  t)

(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-window
         +display-buffer-fallback)))

(after! compile
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (remove-hook 'compilation-filter-hook #'doom-apply-ansi-color-to-compilation-buffer-h))

(after! doom-modeline
  (setq doom-modeline-modal-icon nil))



(defconst +theme-settings
  (let* ((dark-fg "#bbc2cf")
         (light-fg "#556b72")
         (dark-bg "#282c34")
         (light-bg "#FDF6E3")
         (title `((t :height 1.5 :weight normal)))
         (heading `((t :height 1.2)))
         (block-markup `((t :inherit font-lock-comment-face :background unspecified :italic nil :weight light :bold nil)))
         (outline-heading `((t :weight semibold :foreground unspecified)))
         (bullet '((((background light))
                    (:foreground "#96A7A9"))
                   (((background dark))
                    (:foreground "#5B6268")))))
    `((font-lock-comment-face
       ((((background dark))
         (:foreground "#869799" :italic t))))
      (font-lock-doc-face
       ((((background dark))
         (:foreground "#869799" :italic t))))
      (font-lock-constant-face
       ((((background dark))
         (:foreground "#a9a1e1" :weight semibold))
        (((background light))
         (:foreground "#6c71c4" :weight semibold))))
      (parenthesis
       ((((background dark))
         (:foreground "#869799" :italic nil :weight light :inherit unspecified))
        (((background light))
         (:foreground "#9ca0a4" :italic nil :weight light :inherit unspecified))))
      (org-roam-search-highlight
       ((((background dark))
         (:foreground "#98be65" :background "#3e493d" :weight bold :inherit magit-diff-added-highlight))
        (((background light))
         (:foreground "#859900" :background "#e5e3b5" :weight bold :inherit magit-diff-added-highlight))))
      (dirvish-hl-line
       ((((background dark))
         (:background ,(doom-blend dark-bg "#51afef" 0.5) :extend t))
        (((background light))
         (:background ,(doom-blend light-bg "#268bd2" 0.4) :extend t))))
      (org-agenda-clocking
       ((((background dark))
         (:foreground ,dark-fg :extend t))
        (((background light))
         (:foreground ,light-fg :extend t))))
      (magit-section-secondary-heading
       ((((background dark))
         (:foreground "#a9a1e1" :weight semibold :extend t))
        (((background light))
         (:foreground "#6c71c4" :weight semibold :extend t))))
      (magit-tag
       ;; Inversion of normal colours for themes
       ((((background dark))
         (:underline "#b58900"))
        (((background light))
         (:underline "#ECBE7B"))))

      (org-document-info-keyword ((t :italic nil)) t)
      (org-meta-line ((t :inherit font-lock-comment-face :italic nil :foreground unspecified)) t)
      (org-link ((t :weight unspecified :underline nil)) t)
      (org-footnote ((t :foreground unspecified :slant italic :inherit font-lock-comment-face)))
      (compilation-warning ((t :italic nil)))
      (magit-header-line ((t :background unspecified :box nil)))
      (org-document-title ,title)
      (org-roam-header-line ((t :inherit org-document-title)))
      (vertico-group-title ((t :inherit magit-section-heading)))
      (consult-line-number ((t :inherit font-lock-comment-face)))
      (shortdoc-section ((t :inherit fixed-pitch)))
      (org-roam-review-tags-filter ((t :inherit magit-section-heading :bold nil)))
      (org-roam-review-heading ,outline-heading)

      (edebug-enabled-breakpoint ((t :bold t :inherit error)))
      (edebug-disabled-breakpoint ((t :bold t :inherit font-lock-builtin-face)))

      (outline-1 ,outline-heading)
      (outline-2 ,outline-heading)
      (outline-3 ,outline-heading)
      (outline-4 ,outline-heading)
      (outline-5 ,outline-heading)
      (outline-6 ,outline-heading)
      (outline-7 ,outline-heading)
      (outline-8 ,outline-heading)
      (org-roam-title ((t :inherit magit-section-secondary-heading)))
      (org-roam-olp ((t :inherit magit-section-secondary-heading :italic t :bold nil)))
      (org-agenda-structure ,(+append-faces outline-heading heading))
      (link ((t :weight unspecified)) t)
      (highlight ((t :inherit unspecified :foreground unspecified :background unspecified :bold t)))
      (highlight-thing ((t (:inherit highlight))))
      (org-drawer ,block-markup)
      (org-list-dt ((t :italic t :foreground unspecified)))
      (org-agenda-done ((t :inherit org-done :bold nil)) t)
      (org-todo ((t :weight light)))
      (org-done ((t :weight light)))
      (mode-line ((t :inherit default)))
      (font-lock-builtin-face ((t :italic nil)) t)
      (font-lock-keyword-face ((t :weight normal :bold nil)) t)
      (org-block-begin-line ,block-markup)
      (org-block ((t :background unspecified)))
      (org-block-end-line ,block-markup)

      (markdown-header-delimiter-face ,bullet)
      (markdown-table-face ((t :inherit org-table)))
      (markdown-header-face-1 ,title)
      (markdown-header-face-2 ,(+append-faces outline-heading heading))
      (markdown-header-face-3 ,(+append-faces outline-heading '((t :italic t))))
      (markdown-header-face-4 ,(+append-faces outline-heading '((t :underline t)))))))

(apply 'custom-theme-set-faces 'user +theme-settings)

;;; Indent guides

(defconst +indent-guides-enabled-modes nil
  "List of major-modes in which indent-guides will be enabled by default.")

(when (modulep! :ui indent-guides)
  (setq
   indent-bars-pattern "."
   indent-bars-width-frac 0.5
   indent-bars-pad-frac 0.25
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth '(:face default :blend 0.4))

  (add-hook! '+indent-guides-inhibit-functions
    (not (apply #'derived-mode-p +indent-guides-enabled-modes))))


;;; Completion

(map! :after vertico
      :map vertico-map
      "C-k" #'kill-line
      "C-<return>" 'vertico-exit-input
      "M-<return>" 'minibuffer-force-complete-and-exit)
