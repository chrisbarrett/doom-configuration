;;; editor/smartparens-exts/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +sp/add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (cond
       ((and (eq (preceding-char) ?$)
             (equal id "{")))

       ((eq (char-syntax (preceding-char)) ?w)
        (just-one-space))

       ((and (looking-back (sp--get-closing-regexp) (line-beginning-position))
             (not (eq (char-syntax (preceding-char)) ?')))
        (just-one-space))))))

;;;###autoload
(defun +sp/add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(autoload 'org-in-block-p "org")

;;;###autoload
(defun +sp/format-checkitem (_id action context)
  (when (and (eq action 'insert)
             (if (derived-mode-p 'org-mode)
                 (not (org-in-block-p org-list-forbidden-blocks))
               t)
             (string-match-p (rx bos (* space) "-" (* space)
                                 (? "[" (* space) (? "]" (* space)))
                                 (* space)
                                 eos)
                             (buffer-substring (line-beginning-position)
                                               (point))))
    (atomic-change-group
      (just-one-space)
      (search-backward "[" (line-beginning-position))
      (just-one-space)
      (search-forward "]" (line-end-position))
      (just-one-space))))
