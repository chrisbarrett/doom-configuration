;;; lang/org-exts/autoload/+todos.el -*- lexical-binding: t; -*-

(defun +org-todo-list (tags)
  "Show the todo list for the current context.

TAGS are the tags to use when displaying the list."
  (interactive (list (if (org-clocking-p)
                         (list "-someday" "+work")
                       (list "-someday" "-work"))))
  (org-agenda prefix-arg "t")
  (org-agenda-filter-apply (cons "-ignore" tags) 'tag))
