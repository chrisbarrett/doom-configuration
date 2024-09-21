;;; tools/lsp-exts/autoload/eglot-organize-imports-on-save.el -*- lexical-binding: t; -*-

(defgroup eglot-organize-imports-on-save nil
  "Minor mode for organizing imports on save"
  :group 'languages
  :prefix "eglot-organize-imports-on-save-")

(defun eglot-organize-imports-on-save--run ()
  (when (and eglot-organize-imports-on-save-mode
             (eglot-managed-p))
    (save-excursion
      (goto-char (point-min))
      (call-interactively #'eglot-code-action-organize-imports))))

;;;###autoload
(define-minor-mode eglot-organize-imports-on-save-mode
  "Minor mode for automatically organizing imports on save."
  :group eglot-organize-imports-on-save
  (if eglot-organize-imports-on-save-mode
      (add-hook 'before-save-hook #'eglot-organize-imports-on-save--run nil t)
    (remove-hook 'before-save-hook #'eglot-organize-imports-on-save--run t)))
