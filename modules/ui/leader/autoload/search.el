;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))
         (project-root (project-current))))
  (+vertico/project-search nil symbol dir))

;;;###autoload
(defun +search/deadgrep (directory search-term)
  "Search for SEARCH-TERM using ripgrep.

With a prefix argument, search inside DIRECTORY."
  (interactive
   (progn
     (require 'deadgrep)
     (list (when current-prefix-arg
             (read-directory-name "Search in: " ))
           (deadgrep--read-search-term))))
  (deadgrep search-term directory))
