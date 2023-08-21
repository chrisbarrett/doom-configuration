;;; editor/snippets-exts/autoload/+shared.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +yas-bolp ()
  "Non-nil if point is on an empty line or at the first word.
The rest of the line must be blank."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (string-match-p (rx bol (* space) (* word) (* space) eol)
                    line)))

;;;###autoload
(defmacro +yas-line-rx-p (&rest rx-forms)
  `(let ((line (buffer-substring (line-beginning-position) (line-end-position))))
     (string-match-p (rx-to-string '(and ,@rx-forms))
                     line)))


;;;###autoload
(define-advice +snippet--completing-read-uuid (:override (prompt all-snippets &rest args) fix-text-property-access)
  ;; TODO: Remove this advice once fixed upstream.
  ;;
  ;; Related:
  ;;   https://github.com/doomemacs/doomemacs/pull/7359
  ;;   https://github.com/doomemacs/doomemacs/issues/4127
  (let* ((choices
          (cl-loop for (_ . tpl) in (mapcan #'yas--table-templates (if all-snippets
                                                                       (hash-table-values yas--tables)
                                                                     (yas--get-snippet-tables)))

                   for txt = (format "%-25s%-30s%s"
                                     (yas--template-key tpl)
                                     (yas--template-name tpl)
                                     (abbreviate-file-name (yas--template-load-file tpl)))
                   collect
                   (cons txt (yas--template-uuid tpl))))
         (choice (apply #'completing-read prompt choices args)))
    (alist-get choice choices nil nil #'string-equal)))
