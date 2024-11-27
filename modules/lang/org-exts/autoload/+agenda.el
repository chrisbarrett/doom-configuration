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



(defun +agenda--any-scheduled-or-deadline-p ()
  (or (org-get-scheduled-time (point))
      (org-get-deadline-time (point))))

(defun +agenda--skip-heading-safe ()
  (or (outline-next-heading)
      (goto-char (point-max))))

(defun +agenda--scheduled-in-future-p (&optional now)
  (let ((now (or now (current-time))))
    (when-let* ((scheduled (org-get-scheduled-time (point) t)))
      (time-less-p now scheduled))))

(defun +agenda--scheduled-now-p (&optional now)
  (let ((now (or now (current-time))))
    (when-let* ((scheduled (org-get-scheduled-time (point) t)))
      (time-equal-p now scheduled))))

(defun +agenda--at-TODO-p ()
  (equal "TODO" (org-get-todo-state)))

(defun +agenda--first-todo-at-this-level-p ()
  (let (should-skip-entry)
    (unless (+agenda--at-TODO-p)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (+agenda--at-TODO-p)
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
          (setq found (+agenda--scheduled-in-future-p now)))
        found))))



;;;###autoload
(defun +agenda-delegated-section-skip-function ()
  "Skip the item if it has a scheduled or deadline timestamp."
  (when (+agenda--any-scheduled-or-deadline-p)
    (+agenda--skip-heading-safe)))

;;;###autoload
(defun +agenda-tickler-section-skip-function ()
  (when (or (+agenda--scheduled-in-future-p)
            (not (seq-contains-p (org-get-tags) "tickler")))
    (+agenda--skip-heading-safe)))

;;;###autoload
(defun +agenda-view-skip-function ()
  (when (and (seq-contains-p (org-get-tags) "tickler")
             (+agenda--at-TODO-p))
    (+agenda--skip-heading-safe)))

;;;###autoload
(defun +agenda-next-actions-skip-function ()
  (cond
   ;; Don't show things that will naturally show in the agenda.
   ((or (+agenda--any-scheduled-or-deadline-p)
        (+agenda--parent-scheduled-in-future-p))
    (+agenda--skip-heading-safe))

   ;; Always include high-priority todos
   ((and (+agenda--high-priority-p) (+agenda--at-TODO-p))
    nil)

   ((+agenda--first-todo-at-this-level-p)
    (+agenda--skip-heading-safe))))
