;; -*- lexical-binding: t; -*-

(autoload 'xref-push-marker-stack "xref")

;;;###autoload
(defun +jump-to-nix-file ()
  (interactive)
  (let ((default-directory +nix-config-directory))
    (project-find-file)))

;;;###autoload
(defun +jump-to-emacsd-file ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (project-find-file)))

;;;###autoload
(defun +jump-to-doomd-file ()
  (interactive)
  (let ((default-directory doom-user-dir))
    (project-find-file)))

;;;###autoload
(defun +jump-to-init-file ()
  (interactive)
  (find-file (file-name-concat doom-user-dir "init.el")))

;;;###autoload
(defun +jump-to-doom-module (file)
  (interactive (list
                (let* ((all-files
                        (->> doom-modules-dirs
                             (seq-mapcat (fn!
                                          (let ((default-directory %))
                                            (seq-map #'file-relative-name (directory-files-recursively % (rx (or ".org" ".el")))))))
                             (seq-sort #'string<)))
                       (choice (completing-read "File: " all-files nil t)))
                  (expand-file-name
                   choice
                   (seq-find (fn! (file-exists-p (expand-file-name choice %))) doom-modules-dirs)))))
  (xref-push-marker-stack)
  (find-file file))

;;;###autoload
(defun +jump-to-index-file ()
  "Jump to the slipbox index file."
  (interactive)
  (require 'org-roam)
  (org-roam-node-visit (org-roam-node-from-id +roam-index-node-id)))
