;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +search-project-for-symbol-at-point (symbol dir)
  "Search current project for symbol at point.
If prefix ARG is set, prompt for a known project to search from."
  (interactive
   (list (rxt-quote-pcre (or (doom-thing-at-point-or-region) ""))
         (project-root (project-current))))
  (+vertico/project-search nil symbol dir))
