;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +swap-buffers (&optional window)
  (interactive)
  (let* ((buffer-a (window-buffer window))
         (buffer-predicate (frame-parameter (window-frame window) 'buffer-predicate))
         (popup-p (+popup-buffer-p buffer-a)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (car (seq-filter (lambda (buffer-b)
                            (and (not (eq buffer-b buffer-a))
                                 (equal popup-p (+popup-buffer-p buffer-b))
                                 (or (null buffer-predicate)
                                     (funcall buffer-predicate buffer-b))))
                          (seq-map #'car (window-prev-buffers window))))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer buffer-a t)))))
