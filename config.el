;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(dolist (load-dir (list "/run/current-system/sw/share/emacs/site-lisp"
                        "~/.nix-profile/share/emacs/site-lisp"))
  (when (file-directory-p load-dir)
    (add-to-list 'load-path load-dir)))

(setq doom-font (font-spec :family "Fira Code" :size 12))
(setq doom-variable-pitch-font (font-spec :family "Helvetica Neue" :size 12))

(condition-case err
    (setq doom-theme (+theme-for-system-theme))
  (error (warn "Failed to set theme on startup: %s " (error-message-string err))))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org")
(setq org-roam-directory (file-name-concat org-directory "roam"))

(setq +bibfiles (list (file-name-concat org-directory "org-roam.bib")))
(setq +roam-litnotes-paths (list (file-name-concat org-roam-directory "litnotes")))

(setq +roam-index-node-id "0F0670F7-A280-4DD5-8FAC-1DB3D38CD37F")

(after! ediff
  (remove-hook 'ediff-before-setup-hook #'doom-ediff-save-wconf-h)
  (remove-hook 'ediff-quit-hook #'doom-ediff-restore-wconf-h)
  (remove-hook 'ediff-suspend-hook #'doom-ediff-restore-wconf-h))

(after! format-all
  (setq format-all-show-errors 'never))

(after! projectile
  (setq projectile-auto-discover t)
  (add-to-list 'projectile-project-search-path (cons "~/src" 1))
  (add-to-list 'projectile-project-search-path "~/.config"))
