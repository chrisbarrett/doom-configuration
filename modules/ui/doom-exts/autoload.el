;;; ui/doom-exts/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +theme-light ()
  "Enable light colour theme."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'doom-solarized-light t))

;;;###autoload
(defun +theme-dark ()
  "Enable dark colour theme."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'doom-one t))

(defun +gtk-system-theme ()
  (with-temp-buffer
    (ignore-errors
      (call-process "gsettings" nil t nil "get" "org.gnome.desktop.interface" "gtk-theme"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

(defun +macos-system-theme ()
  (with-temp-buffer
    (ignore-errors
      (call-process "defaults" nil t nil "read" "-g" "AppleInterfaceStyle"))
    (if (string-match-p "dark" (buffer-string))
        'dark
      'light)))

;;;###autoload
(defun +theme-for-system-theme ()
  (when-let* ((change-fn (pcase system-type
                           (`darwin #'+macos-system-theme)
                           (`gnu/linux #'+gtk-system-theme))))
    (pcase (funcall change-fn)
      (`dark 'doom-one)
      (`light 'doom-solarized-light))))

;;;###autoload
(defun +append-faces (&rest specs)
  "Smash SPECS together."
  (require 'ht)
  `((t ,@(ht-to-plist (seq-reduce (pcase-lambda (acc `((,_pred . ,attrs)))
                                    (ht-merge acc (ht-from-plist attrs)))
                                  specs
                                  (ht-create))))))
