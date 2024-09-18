;;; editor/snippets-exts/autoload/+util.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +snippets-each-file (fn)
  "Iterate over snippets calling FN on each.

FN is called with a single argument, which is the absolute path
of the current snippet. It is called in a temp buffer with that
snippet's contents."
  (dolist (file (directory-files-recursively +snippets-dir (rx (* any))))
    (with-temp-buffer
      (let ((require-final-newline nil))
        (insert-file-contents file)
        (funcall fn file)))))

;;;###autoload
(defun +snippet-filename-upper-camel-case ()
  (s-upper-camel-case (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))

;;;###autoload
(defun +snippet-filename-kebab-case ()
  (s-dashed-words (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
