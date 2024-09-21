;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(map! "M-s" #'string-inflection-all-cycle
      "M-f" #'unfill-toggle

      :n "z SPC" #'ispell-word

      ;; Global bindings to match what I have in terminals.
      :gniv "C-t" 'project-find-file
      :gniv "C-/" '+vertico/project-search
      (:after evil-collection-magit
       :map 'magit-status-mode-map
       :niv "C-t" nil))

(setq org-directory "~/org")
(setq org-roam-directory (file-name-concat org-directory "roam"))
(setq +bibfiles (list (file-name-concat org-directory "org-roam.bib")))
(setq +roam-litnotes-paths (list (file-name-concat org-roam-directory "litnotes")))
(setq +roam-index-node-id "0F0670F7-A280-4DD5-8FAC-1DB3D38CD37F")

(setq +git-auto-commit-dirs (list org-directory))

(setq ispell-dictionary "en_GB")

(add-hook! (bash-ts-mode
            docker-ts-mode
            java-ts-mode
            json-mode
            json-ts-mode
            markdown-mode
            nix-mode
            rust-ts-mode
            rustic-mode
            typescript-ts-mode
            yaml-ts-mode
            zig-mode)
           #'lsp!)

(add-hook! (java-ts-mode
            typescript-ts-mode)
           #'eglot-organize-imports-on-save-mode)


(setq doom-font (font-spec :family "Fira Code" :size 12))
(setq doom-variable-pitch-font (font-spec :family "Helvetica Neue" :size 12))

(with-demoted-errors "Error enabling theme on startup: %S"
  (+theme-update))

(setq-hook! (dired-mode treemacs-mode)
  display-line-numbers nil)

(defconst +indent-significant-lang-modes
  '(yaml-mode yaml-ts-mode nxml-mode python-ts-mode))

(add-hook! '+indent-guides-inhibit-functions
  (not (apply #'derived-mode-p +indent-significant-lang-modes)))


(defconst +project-discovery-dirs
  `("~/.config/"
    "~/src/"
    ,@(seq-filter #'file-directory-p
                  (seq-difference (directory-files "~/src/") '("." "..")))))

(dolist (dir +project-discovery-dirs)
  (project-remember-projects-under dir))

(setq vc-directory-exclusion-list
      '("node_modules"
        "cdk.out"
        "target"
        ".direnv"
        ".git"))

(setq envrc-show-summary-in-minibuffer nil)


(map! "C-SPC" #'consult-buffer
      (:after winner
       :n "C-." nil)
      (:after winner
       :map winner-mode-map
       :gvni "C-," #'winner-undo
       :gvni "C-." #'winner-redo))

(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq help-window-select t)
