;;; lang/typescript/autoload.el -*- lexical-binding: t; -*-
;;
;; TypeScript code evaluation with Bun. Adapted from ob-js.

(cl-eval-when (compile)
  (require 'ob-comint)
  (require 'ob))

(autoload 'org-babel-js-var-to-js "ob-js")

(defvar ob-bun-program "bun")

(defconst ob-bun-function-wrapper
  "require('process').stdout.write(require('util').inspect((() => {
%s
})()));")

;;;###autoload
(defun org-babel-variable-assignments:typescript (params)
  (mapcar (pcase-lambda (`(,ident . ,value))
            (format "let %s = %s;" ident
                    (org-babel-js-var-to-js value)))
          (org-babel--get-vars params)))

(defun ob-bun--eval-via-tmp-file (body params)
  (let* ((result-type (alist-get :result-type params))
         (script-file (org-babel-temp-file "ts-script-"))
         (expanded (org-babel-expand-body:generic body
                                                  params
                                                  (org-babel-variable-assignments:typescript params)))
         (expanded (if (string= result-type "value")
                       (format ob-bun-function-wrapper expanded)
                     expanded)))
    (with-temp-file script-file
      (insert expanded))
    (org-babel-eval (concat ob-bun-program " " (org-babel-process-file-name script-file))
                    "")))

;;;###autoload
(defun org-babel-execute:typescript (body params)
  (let ((result (if (equal "none" (alist-get :session params))
                    (ob-bun--eval-via-tmp-file body params)
                  (error "Sessions not supported for typescript"))))
    (org-babel-result-cond (alist-get :result-params params)
      result
      (org-babel-js-read result))))

