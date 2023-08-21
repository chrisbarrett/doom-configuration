;; -*- lexical-binding: t; -*-

;;;###autoload
(defun eshell/j (&rest query)
  "Jump to a directory with fasd QUERY."
  (let* ((command `("fasd" "-ld" ,@(mapcar #'shell-quote-argument query)))
         (output (shell-command-to-string (string-join command " ")))
         (matches (nreverse (split-string output "\n" t))))
    (if-let* ((dir (car matches)))
        (eshell/cd dir)
      (let ((message-log-max))
        (message "No fasd match")))))

;;;###autoload
(defun eshell/g ()
  "Navigate to the Git root."
  (let (message-log-max)
    (if-let* ((dir (locate-dominating-file default-directory ".git")))
        (progn
          (message "Moving to git repository root")
          (eshell/cd dir))
      (if-let* ((dir (project-root)))
          (progn
            (message "Moving to project root")
            (eshell/cd dir))
        (user-error "Not in a project or git repo")))))
