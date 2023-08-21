;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +delete-current-buffer-and-file ()
  "Remove the file associated with the current buffer, then kill it."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((null file)
      (kill-buffer))
     ((not (file-exists-p file))
      (kill-buffer))
     ((yes-or-no-p "Delete this file? ")
      (delete-file file t)
      (kill-buffer)
      (message "File deleted: %s" file)))))

;;;###autoload
(defun +reload-file ()
  "Revisit the current file."
  (interactive)
  (when-let* ((path (buffer-file-name)))
    (find-alternate-file path)))

;;;###autoload
(defun +copy-buffer-directory ()
  "Show and copy the directory of the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (if-let* ((path (or (ignore-errors (file-name-directory (buffer-file-name))) list-buffers-directory)))
      (progn
        (kill-new path)
        (message "%s" path))
    (error "Buffer not visiting a file")))
