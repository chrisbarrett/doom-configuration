;;; config/default-exts/autoload/+seq.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +seq-max-by (comparator sequence)
  (seq-reduce (lambda (acc it)
                (if (funcall comparator it acc)
                    it
                  acc))
              sequence
              nil))

;;;###autoload
(defun +seq-min-by (comparator sequence)
  (seq-reduce (lambda (acc it)
                (if (funcall comparator it acc)
                    acc
                  it))
              sequence
              nil))

;;;###autoload
(defun +seq-shuffle (sequence)
  (seq-sort-by (lambda (_) (random)) #'<= sequence))
