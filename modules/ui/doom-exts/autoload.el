;;; ui/doom-exts/autoload.el -*- lexical-binding: t; -*-

(cl-defgeneric +system-theme-query (system-type))

(cl-defmethod +system-theme-query ((_ (eql 'darwin)))
  (shell-command-to-string "defaults read -g AppleInterfaceStyle"))

(cl-defmethod +system-theme-query ((_ (eql 'gnu/linux)))
  (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme"))

(defun +system-theme ()
  (if (string-match-p "dark" (+system-theme-query system-type))
      'dark
    'light))

;;;###autoload
(defun +theme-for-system-theme ()
  (pcase (+system-theme)
    (`dark 'doom-one)
    (`light 'doom-solarized-light)))

;;;###autoload
(defun +theme-update ()
  "Sync the Emacs theme with the system."
  (let* ((inhibit-redisplay t)
         (updated-theme (+theme-for-system-theme)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme updated-theme t)))

;;;###autoload
(defun +append-faces (&rest specs)
  "Smash SPECS together."
  (require 'ht)
  `((t ,@(ht-to-plist (seq-reduce (pcase-lambda (acc `((,_pred . ,attrs)))
                                    (ht-merge acc (ht-from-plist attrs)))
                                  specs
                                  (ht-create))))))
