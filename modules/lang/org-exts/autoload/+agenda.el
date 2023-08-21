;;; +agenda.el --- Utility functions for agenda views  -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for my org-agenda configuration.

;;; Code:

;;;###autoload
(defun +agenda-dwim ()
  "Show either the personal or work agenda."
  (interactive)
  (require 'org)
  (org-agenda nil (if (org-clocking-p) "w" "p")))

(defun +agenda--scheduled-or-deadline-p ()
  (or (org-get-scheduled-time (point))
      (org-get-deadline-time (point))))

(defun +agenda--skip-heading-safe ()
  (or (outline-next-heading)
      (goto-char (point-max))))

(defun +agenda--skipping-ignored-p ()
  (when-let* ((prop (org-entry-get-with-inheritance "AGENDA_SKIP")))
    (cond
     ((string-match-p (rx bos "ignore" eos) prop)
      t)
     ((and (string-match-p (rx bos "scheduled" eos) prop)
           (not (org-get-scheduled-time (point) t)))
      nil
      t))))

;;;###autoload
(defun +agenda-skip-item-if-timestamp ()
  "Skip the item if it has a scheduled or deadline timestamp."
  (when (+agenda--scheduled-or-deadline-p)
    (+agenda--skip-heading-safe)))

(defun +agenda--current-headline-is-todo ()
  (equal "TODO" (org-get-todo-state)))

(defun +agenda--first-todo-at-this-level-p ()
  (let (should-skip-entry)
    (unless (+agenda--current-headline-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (+agenda--current-headline-is-todo)
          (setq should-skip-entry t))))
    should-skip-entry))

(defun +agenda--high-priority-p ()
  (equal ?A (nth 3 (org-heading-components))))

(defun +agenda--parent-scheduled-in-future-p ()
  (save-restriction
    (widen)
    (save-excursion
      (let ((found)
            (now (current-time)))
        (while (and (not found) (org-up-heading-safe))
          (when-let* ((scheduled (org-get-scheduled-time (point) t)))
            (when (time-less-p now scheduled)
              (setq found t))))
        found))))

;;;###autoload
(defun +agenda-skip-items-already-shown ()
  (cond
   ((+agenda--skipping-ignored-p)
    nil)

   ;; Don't show things that will naturally show in the agenda.
   ((or (+agenda--scheduled-or-deadline-p) (+agenda--parent-scheduled-in-future-p))
    (+agenda--skip-heading-safe))

   ((and (+agenda--high-priority-p) (+agenda--current-headline-is-todo))
    ;; Show these items.
    nil)

   ((+org-project-p)
    (+org-project-skip-stuck-projects))

   ((+agenda--first-todo-at-this-level-p)
    (+agenda--skip-heading-safe))))
