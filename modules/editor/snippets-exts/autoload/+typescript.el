;;; editor/snippets-exts/autoload/+typescript.el -*- lexical-binding: t; -*-

(defcustom +yas-js-import-binding-to-module-name-alist '()
  "Map the name of a default import to a module.

Expected to be set via directory variable."
  :type '(alist :key-type string :value-type string)
  :group 'snippets-exts
  :safe (fn! (and (listp %)
                  (seq-every-p #'car #'stringp)
                  (seq-every-p #'cdr #'stringp))))

;;;###autoload
(cl-defun +yas-js-module-name-for-binding (&optional (text yas-text))
  (pcase (when (stringp text)
           (string-trim text))
    ('nil "")
    ("" "")
    ((guard (assoc text +yas-js-import-binding-to-module-name-alist))
     (alist-get text +yas-js-import-binding-to-module-name-alist))
    ("VError" "verror")
    ("memoize" "lodash")
    ("_" "lodash")
    ("z" "zod")
    ("cdk" "aws-cdk-lib")
    ("dynamodb" "aws-cdk-lib/aws-dynamodb")
    ("lambda" "aws-cdk-lib/aws-lambda")

    ((guard (string-match-p "{" text))
     (-let [(_ inner) (s-match (rx "{" (* space) (+? nonl) (* space) "}") text)]
       (+yas-js-module-name-for-binding inner)))
    (s
     (-if-let* ((match-binding (rx (* space) "*" (+ space) "as" (+ space) (group (+ (not (any space))))))
                ((_ name) (s-match match-binding text)))
         (+yas-js-module-name-for-binding name)
       (downcase (s-dashed-words s))))))

;;;###autoload
(defun +yas-js-buffer-imports-logger-p ()
  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
    (string-match-p (rx bol "import" symbol-end (+? nonl) (or "winston" "logger")) str)))

;;;###autoload
(defun +yas-js-inside-describe-p ()
  (save-excursion
    (search-backward-regexp (rx bol (* space) symbol-start "describe" symbol-end) nil t)))
