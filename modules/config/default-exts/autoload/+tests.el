;;; config/default-exts/autoload/+tests.el -*- lexical-binding: t; -*-

;; Provides a nicer syntax for defining tests to run via ERT. It uses similar
;; BDD-style tests to `buttercup', but since it uses ERT it is better-suited to
;; interactive development.

(require 'cl-lib)

;;;###autoload
(cl-eval-when (compile load)
  (defvar +describe--stack nil
    "Dynamic variable used to store describe descriptions.

It is managed by `+describe' and `+it' and should not be set
manually."))

;;; TODO: Building the describe stack isn't working yet

;;;###autoload
(defmacro +describe (desc &rest body)
  "Describe an ERT test suite, using a nested syntax.

DESC is a string. BODY is a sequence of instructions, mainly
calls to `+describe' and `+it'."
  (declare (indent 1) (debug (&define sexp def-body)))
  (cl-assert (or (stringp desc) (symbolp desc)))
  (let ((+describe--stack (cons desc +describe--stack)))
    (eval `(cl-eval-when (eval)
             ,@body))))

;;;###autoload
(defmacro +it (desc &rest body)
  "Define an ERT test.

Uses the stack of enclosing `+describe' blocks to build the suite
name.

DESC is a string. BODY is a sequence of instructions, and is
interpreted within the context of an ERT test run.

Use `should', `should-not' and `should-error' to write
assertions."
  (declare (indent 1) (debug (&define sexp def-body)))
  (cl-assert (or (stringp desc) (symbolp desc)))
  (let* ((descs (seq-reverse
                 (seq-map (lambda (it) (format "%s" it))
                          (cons desc +describe--stack))))
         (test-name (intern
                     (string-replace " " "-" (string-join descs "--")))))
    `(progn
       (ert-deftest ,test-name () ,@body)
       nil)))


;;; Example test suite

;; (+describe "outer"
;;   (+it "foo")
;;   (+it "bar")
;;   (+describe "inner"
;;     (+it "foo"
;;       (should (equal 1 2)))
;;     (+it "bar")))
