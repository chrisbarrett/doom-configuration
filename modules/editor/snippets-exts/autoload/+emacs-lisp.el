;;; editor/snippets-exts/autoload/+emacs-lisp.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +yas-emacs-lisp-custom-group ()
  "Find the first group defined in the current file.

Fall back to the file name sans extension."
  (or
   (cadr (s-match (rx "(defgroup" (+ space) (group (+ (not space))))
                  (buffer-string)))
   (cadr (s-match (rx ":group" (+ space) "'" (group (+ (any "-" alnum))))
                  (buffer-string)))
   (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

;;;###autoload
(defun +yas-emacs-lisp-autoload-file (sym)
  (if-let* ((file (symbol-file (if (stringp sym) (intern sym) sym))))
      (file-name-sans-extension (file-name-nondirectory file))
    ""))

;;;###autoload
(defun +yas-emacs-lisp-at-line-above-decl-p ()
  (save-excursion
    (forward-line)
    (back-to-indentation)
    (thing-at-point-looking-at (rx (* space) "("
                                   (or "cl-defun" "defun" "defvar" "defconst"
                                       "defmacro"
                                       "cl-defmacro"
                                       "define-minor-mode"
                                       "define-globalized-minor-mode"
                                       "define-derived-mode")))))

;;;###autoload
(defun +yas-emacs-lisp-package-prefix ()
  (cond
   ((string-prefix-p "*Org Src" (buffer-name))
    "")
   ((bound-and-true-p nameless-current-name)
    (format "%s-" nameless-current-name))
   (t
    (format "%s-" (file-name-base (or (buffer-file-name) (buffer-name)))))))

;;;###autoload
(defun +yas-emacs-lisp-buttercup-file-p ()
  (string-match-p "^test-" (file-name-nondirectory (buffer-file-name))))

;;;###autoload
(defun +yas-emacs-lisp-doom-file-p ()
  (or (seq-find (fn! (f-descendant-of-p default-directory %))
                doom-modules-dirs)
      (equal default-directory doom-user-dir)))
