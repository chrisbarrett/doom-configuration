;;; quail-util.el -*- lexical-binding: t; -*-

(defmacro +quail-defun (package-name key &rest body)
  ;; https://emacs.stackexchange.com/questions/76725/how-to-implement-a-function-in-quail-define-rules-for-set-input-method
  (declare (indent 2))
  (cl-assert (stringp key))
  (cl-assert (stringp package-name))
  (let ((gkey (gensym))
        (gpackage-name (gensym)))
    `(let* ((,gkey ,key)
            (,gpackage-name ,package-name)
            (fname (make-symbol (format "+quail-%s-key-%s" ,gpackage-name ,gkey))))
       (defalias fname (lambda (key idx)
                         (quail-delete-region)
                         (setq quail-current-str nil
                               quail-converting nil
                               quail-conversion-str "")
                         (atomic-change-group ,@body)
                         (throw 'quail-tag nil)))

       (quail-defrule ,gkey fname ,gpackage-name t))))
