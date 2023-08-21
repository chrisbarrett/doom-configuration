;; -*- lexical-binding: t; -*-

(defun +org-cite-collapse--citation-ov-at (pt)
  (seq-find (lambda (ov) (equal 'org-pretty-citation (overlay-get ov 'type)))
            (overlays-at pt)))

(defun +org-cite-collapse--activate-citation-ov (ov)
  (overlay-put ov 'priority 2000)
  (overlay-put ov 'evaporate t)
  (overlay-put ov 'invisible t)
  (overlay-put ov 'display "[@]")
  (overlay-put ov 'type 'org-pretty-citation))

(defun +org-cite-collapse--deactivate-citation-ov (ov)
  (overlay-put ov 'invisible nil)
  (overlay-put ov 'display nil))

(defun +org-cite-collapse--citation-cursor-sensor-function (_window prev-pt action)
  (pcase action
    ('entered
     (when-let* ((ov (+org-cite-collapse--citation-ov-at (point))))
       (+org-cite-collapse--deactivate-citation-ov ov)))
    ('left
     (when-let* ((ov (+org-cite-collapse--citation-ov-at prev-pt)))
       (+org-cite-collapse--activate-citation-ov ov)))))

;;;###autoload
(defun +org-cite-collapse-activation-function (citation)
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    (unless (or (org-in-regexp (rx bol ":" (* nonl)))
                (+org-cite-collapse--citation-ov-at beg))
      (let ((ov (make-overlay beg end nil t)))
        (overlay-put ov 'cursor-sensor-functions '(+org-cite-collapse--citation-cursor-sensor-function))
        (+org-cite-collapse--activate-citation-ov ov)))))
