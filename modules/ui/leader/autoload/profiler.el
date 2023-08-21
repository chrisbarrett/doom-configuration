;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +profiler-stop-and-report (&optional continue-p)
  "Stop the profiler and show results.

With optional prefix arg CONTINUE-P, keep profiling."
  (interactive "P")
  (let ((ran-p (profiler-running-p)))

    (unless continue-p
      (profiler-stop))
    (profiler-report)
    (when ran-p
      (if continue-p
          (message "Profiler still recording")
        (message "Profiler stopped")))))
