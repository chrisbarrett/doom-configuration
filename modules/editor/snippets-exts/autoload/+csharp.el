;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +yas-csharp-expression-context-p (regexp)
  (thing-at-point-looking-at (rx-to-string `(and (or "=>"
                                                     (any ",;()=:"))
                                                 (* space) (regexp ,regexp) (* space)))))
