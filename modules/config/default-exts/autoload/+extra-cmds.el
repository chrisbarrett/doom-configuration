;;; config/default-exts/autoload/+extra-cmds.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +insert-uuid (&optional upcase-p)
  "Insert a UUID at point.

Prefix arg UPCASE-P determines whether the UUID is downcased or
upcased on insertion."
  (interactive "*P")
  (let ((uuid (string-trim (shell-command-to-string "uuidgen"))))
    (insert (if upcase-p (upcase uuid) (downcase uuid)))))

;;;###autoload
(defun +insert-date (&optional prompt)
  "Read a date string interactively and insert it at point."
  (interactive "*P")
  (insert (if prompt
              (let ((formats (seq-map #'format-time-string
                                      '("%F"
                                        "%F %R"
                                        "%X"
                                        "%c"))))
                (completing-read "Format: " formats nil t))
            (format-time-string "%F"))))

;;;###autoload
(defun +remove-sgr-sequences ()
  "Remove SGR sequences from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\x1B\\[\\([0-9]+;?\\)*[m|K|G]" nil t)
      (replace-match ""))))
