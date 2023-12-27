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
                                       "eval-when-compile"
                                       "eval-and-compile"
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
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((result (or
                   (string-match-p (rx bol (* space) "(" (or "describe" "it") symbol-end)
                                   (buffer-string))
                   (when-let* ((filename (buffer-file-name)))
                     (string-match-p "^test-" (file-name-nondirectory filename))))))

      (numberp result))))

;;;###autoload
(defun +yas-emacs-lisp-doom-file-p ()
  (or (seq-find (fn! (f-descendant-of-p default-directory %))
                doom-modules-dirs)
      (equal default-directory doom-user-dir)))

;;;###autoload
(defun +yas-emacs-lisp-in-describe-p ()
  (save-excursion
    (let ((found (thing-at-point-looking-at (rx "(+describe" (+ space))))
          (at-top (zerop (syntax-ppss-depth (syntax-ppss)))))
      (if (or found at-top)
          found
        (backward-up-list)
        (+yas-emacs-lisp-in-describe-p)))))
