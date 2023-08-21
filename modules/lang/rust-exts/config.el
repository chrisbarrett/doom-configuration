;; -*- lexical-binding: t; -*-

(after! (:and rustic-babel ob)
  ;; Add a `:profile' header arg for specifying the build profile.
  (define-advice rustic-make-process (:filter-args (args) apply-profile)
    (setf (plist-get args :command)
          (append
           (plist-get args :command)
           (when-let* ((profile (alist-get :profile rustic-babel-params)))
             (list (format "--%s" profile)))))
    args))
