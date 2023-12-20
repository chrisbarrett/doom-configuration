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
