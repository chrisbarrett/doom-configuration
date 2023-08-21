;; -*- lexical-binding: t; -*-

(autoload 'flymake--diagnostics-buffer-name "flymake")
(autoload 'project-root "project")

;;;###autoload
(defun +flymake-toggle-buffer-error-list ()
  "Show or hide the buffer error list."
  (interactive)
  (if-let* ((window (seq-find (lambda (it)
                                (equal (flymake--diagnostics-buffer-name)
                                       (buffer-name (window-buffer it))))
                              (window-list))))
      (delete-window window)
    (flymake-show-buffer-diagnostics)))

;;;###autoload
(defun +flymake-toggle-project-error-list ()
  "Show or hide the project error list."
  (interactive)
  (if-let* ((window (seq-find (lambda (it)
                                (equal (format "*Flymake diagnostics for `%s'*" (project-root (project-current)))
                                       (buffer-name (window-buffer it))))
                              (window-list))))
      (delete-window window)
    (flymake-show-project-diagnostics)))
