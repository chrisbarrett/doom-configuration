;;; config/default-exts/autoload/+url.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +last-url-kill ()
  "Return the most recent URL in the kill ring or X pasteboard."
  (seq-find (fn! (string-match-p (rx bos (or "http" "https" "www")) %))
             (ignore-errors
               (cons (current-kill 0 t) kill-ring))))

(defun +url--strip-google-highlight-query-param (url)
  (car (split-string url (rx (? "#") ":~:"))))

;;;###autoload
(defun +read-url (&optional prompt default)
  (let* ((default (-some->> (or default (thing-at-point-url-at-point) (+last-url-kill))
                    (string-trim)))
         (prompt (or prompt "URL"))
         (input (read-string (concat (if default (format "%s (default %s)" prompt default) prompt) ": ")
                             nil nil default)))
    (substring-no-properties
     (if (string-match-p (rx "http" (? "s") "://") input)
         (+url--strip-google-highlight-query-param input)
       (+read-url prompt default)))))
