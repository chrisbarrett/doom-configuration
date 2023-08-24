;;; lang/org-exts/autoload/+links.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +link--prepend-custom-icon (icon &optional prefix)
  (lambda (start _end _url _bracketed-p)
    (when prefix
      (add-text-properties start (+ start (length prefix)) 'display 'invisible))
    (add-text-properties
     start (1+ start)
     (list 'display
           (concat
            (propertize icon
                        'face
                        '(:weight light
                          :height 0.8
                          :ascent 0.1
                          :inherit success))
            (propertize " " 'face '(:height 0.5)))))))
