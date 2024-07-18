;;; ui/leader/autoload/projects.el -*- lexical-binding: t; -*-

(persist-defvar +project-test-commands nil
  "An alist of project roots to compilation commands.")

(cl-defgeneric +project-default-test-command (project))

(cl-defmethod +project-default-test-command (_) nil)

(defvar +projects-default-test-command-fallback "make test")

(defun +project-test-command (project)
  (alist-get (project-root project) +project-test-commands
             (+project-default-test-command project)))

(defun +project--update-test-command (project command)
  (setf (alist-get (project-root project) +project-test-commands)
        command))

;;;###autoload
(defun +project-test (&optional arg)
  "Run a test command in the project root.

With optional prefix argument ARG, re-prompt for the test command
if one has already been saved."
  (declare (interactive-only test))
  (interactive "P")
  (let* ((project (project-current t))
         (prev-command (+project-test-command project))
         (command (if (or arg (null prev-command))
                      (read-string "Test command: "
                                   (or prev-command
                                       +projects-default-test-command-fallback))
                    prev-command)))

    (+project--update-test-command project command)

    (let ((default-directory (project-root project))
          (compilation-buffer-name-function
           (lambda (_) (format "*test*<%s>" (project-name project)))))
      (compile command))))



(persist-defvar +project-compile-commands nil
  "An alist of project roots to compilation commands.")

(cl-defgeneric +project-default-compile-command (project))

(cl-defmethod +project-default-compile-command (_) nil)

(defvar +projects-default-compile-command-fallback "make -k")

(defun +project-compile-command (project)
  (alist-get (project-root project) +project-compile-commands
             (+project-default-compile-command project)))

(defun +project--update-compile-command (project command)
  (setf (alist-get (project-root project) +project-compile-commands)
        command))

;;;###autoload
(defun +project-compile (&optional arg)
  "Run a compile command in the project root.

With optional prefix argument ARG, re-prompt for the compile command
if one has already been saved."
  (declare (interactive-only compile))
  (interactive "P")
  (let* ((project (project-current t))
         (prev-command (+project-compile-command project))
         (command (if (or arg (null prev-command))
                      (read-string "Compile command: " (or prev-command
                                                           +projects-default-compile-command-fallback))
                    prev-command)))

    (+project--update-compile-command project command)

    (let ((default-directory (project-root project))
          (compilation-buffer-name-function
           (lambda (_) (format "*compilation*<%s>" (project-name project)))))
      (compile command))))
