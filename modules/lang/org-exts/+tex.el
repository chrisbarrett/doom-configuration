;;; lang/org-exts/+tex.el -*- lexical-binding: t; -*-

(after! org

  ;; Use Tectonic as org-mode's primary LaTeX compiler
  (setq org-latex-compiler (getenv "NIX_EMACS_TEX_PROGRAM"))
  (setq org-latex-compilers (list (getenv "NIX_EMACS_TEX_PROGRAM")))
  (setq org-latex-pdf-process (list (concat (getenv "NIX_EMACS_TEX_PROGRAM") " -Z shell-escape --outdir=%o %f")))

  ;; Make latex previews look good on high-DPI screens
  (setq org-preview-latex-default-process 'dvisvgm)
  (plist-put org-format-latex-options :scale 1.6)

  ;; Make C-c C-c toggle LaTeX fragment preview for editing

  (add-hook! 'org-ctrl-c-ctrl-c-hook
    (when (or (+org-at-latex-preview-p) (+org-at-latex-fragment-p))
      (org-latex-preview)))

  ;; Set default LaTeX packages
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-latex-default-packages-alist '("colorlinks=true" "hyperref" nil))

  ;; Don't apply org-block face to latex snippets.
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

  ;; Hide space after hidden keywords.
  (define-advice org-fontify-meta-lines-and-blocks-1 (:after (&rest _) hide-space)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp (rx-to-string `(and bol
                                                       (group "#+" (or "title" "author") ":" (+ space))
                                                       (+ nonl))
                                                 t)
                                   nil t)
        (add-text-properties (match-beginning 1) (match-end 1)
                             '(font-lock-fontified t invisible t))))))
