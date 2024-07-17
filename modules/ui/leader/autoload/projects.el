;;; ui/leader/autoload/projects.el -*- lexical-binding: t; -*-

(persist-defvar +project-test-commands nil
  "An alist of project roots to compilation commands.")

(cl-defgeneric +project-test-command (project)
  (alist-get (project-root project) +project-test-commands
             "make test"))

;;;###autoload
(defun +project-test (&optional arg)
  "Run a test command in the project root.

With optional prefix argument ARG, re-prompt for the test command
if one has already been saved."
  (declare (interactive-only compile))
  (interactive "p")
  (let* ((project (project-current t))
         (prev-command (+project-test-command project))
         (command (if (or arg (null prev-command))
                      (read-string "Test command: " prev-command)
                    prev-command)))

    ;; Update stored command
    (setf (alist-get (project-root project)
                     +project-test-commands)
          command)

    (let ((default-directory (project-root project))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (compile command))))

(persist-defvar +project-compile-commands nil
  "An alist of project roots to compilation commands.")

(cl-defgeneric +project-compile-command (project)
  (alist-get (project-root project) +project-compile-commands
             "make -k"))

;;;###autoload
(defun +project-compile (&optional arg)
  "Run a compile command in the project root.

With optional prefix argument ARG, re-prompt for the compile command
if one has already been saved."
  (declare (interactive-only compile))
  (interactive "p")
  (let* ((project (project-current t))
         (prev-command (+project-compile-command project))
         (command (if (or arg (null prev-command))
                      (read-string "Compile command: " prev-command)
                    prev-command)))

    ;; Update stored command
    (setf (alist-get (project-root project)
                     +project-compile-commands)
          command)

    (let ((default-directory (project-root project))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (compile command))))
