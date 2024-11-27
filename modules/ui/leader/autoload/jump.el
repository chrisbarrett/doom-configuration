;; -*- lexical-binding: t; -*-

(autoload 'xref-push-marker-stack "xref")

(defvar project-current-directory-override)

;;;###autoload
(defun +jump-to-nix-file ()
  (interactive)
  (let ((project-current-directory-override +nix-config-directory))
    (project-find-file)))

;;;###autoload
(defun +jump-to-emacsd-file ()
  (interactive)
  (let ((project-current-directory-override user-emacs-directory))
    (project-find-file)))

;;;###autoload
(defun +jump-to-doomd-file ()
  (interactive)
  (let ((project-current-directory-override doom-user-dir))
    (project-find-file)))

;;;###autoload
(defun +jump-to-doomd-init-file ()
  (interactive)
  (find-file (file-name-concat doom-user-dir "init.el")))

;;;###autoload
(defun +jump-to-doomd-config-file ()
  (interactive)
  (find-file (file-name-concat doom-user-dir "config.el")))

;;;###autoload
(defun +jump-to-doom-module (file)
  (interactive (list
                (let* ((all-files
                        (->> doom-module-load-path
                             (seq-mapcat (fn!
                                          (let ((default-directory %))
                                            (seq-map #'file-relative-name (directory-files-recursively % (rx (or ".org" ".el")))))))
                             (seq-sort #'string<)))
                       (choice (completing-read "File: " all-files nil t)))
                  (expand-file-name
                   choice
                   (seq-find (fn! (file-exists-p (expand-file-name choice %))) doom-module-load-path)))))
  (xref-push-marker-stack)
  (find-file file))

;;;###autoload
(defun +jump-to-index-file ()
  "Jump to the slipbox index file."
  (interactive)
  (require 'org-roam)
  (org-roam-node-visit (org-roam-node-from-id +roam-index-node-id)))
