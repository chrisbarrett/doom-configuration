;;; emacs/dired-exts/config.el -*- lexical-binding: t; -*-

(map! :map dired-mode-map
      :after dired
      :n "M-w" #'dired-copy-filename-as-kill
      :n "," #'dired-hide-details-mode
      :n "." #'dired-omit-mode
      [remap projectile-switch-project] (cmd!
                                         (require 'projectile)
                                         (projectile-completing-read
                                          "Go to project: " (projectile-relevant-known-projects)
                                          :action #'dired)))

(map! :map dirvish-mode-map
      :after dirvish
      :n "TAB" #'dirvish-toggle-fullscreen
      [remap delete-other-windows] #'dirvish-toggle-fullscreen)

(map! :map wdired-mode-map
      :after wdired
      :nv "^" #'evil-first-non-blank
      "C-c C-c" #'wdired-exit
      "C-c C-e" #'wdired-exit)

(after! dired
  (setq dired-clean-confirm-killing-deleted-buffers nil))

(after! dirvish
  (setq dirvish-hide-details t)
  (setq dirvish-vscode-icon-size 12)
  (setq dirvish-attributes '(vscode-icon file-size symlink-target)))

(after! dired-x
  (setq dired-omit-verbose nil))

(after! vscode-icon
  (push '("jpg" . "image") vscode-icon-file-alist))

(after! dirdfl
  (set-face-attribute 'diredfl-dir-name nil :bold t))
