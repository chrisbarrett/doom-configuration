;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;
;; Settings I may need to tweak from time-to time are set here. This helps me
;; locate them them easily with a key sequence, rather than having to dig
;; through Doom modules.


;;; Keybindings

(map!
 :g "C-x SPC" 'insert-char
 :g "C-c SPC" (cmd! (insert-char #x00A0)) ; insert non-breaking space

 :mn "C-|" 'harpoon-quick-menu-hydra
 :mn "C-s" 'harpoon-add-file

 :mn "z SPC" '+spell/correct

 :g "C-@" 'embark-act

 ;; Global bindings to match what I have in terminals.
 :gniv "C-t"  #'project-find-file
 :gniv "C-/"  #'+vertico/project-search

 (:after evil-collection-magit
  :map 'magit-status-mode-map
  :niv "C-t" nil))


;;; org-mode

(setq
 org-directory          "~/org"
 org-roam-directory     (file-name-concat org-directory "roam")
 +bibfiles              (list (file-name-concat org-directory "org-roam.bib"))
 +roam-litnotes-paths   (list (file-name-concat org-roam-directory "litnotes"))
 +roam-index-node-id    "0F0670F7-A280-4DD5-8FAC-1DB3D38CD37F"
 +git-auto-commit-dirs  (list org-directory)
 ispell-dictionary      "en_GB")


;;; Input methods

(setq default-input-method "french-postfix")

(load! "+quail")

(with-eval-after-load "quail/latin-post"
  (message "Initializing custom keybindings for latin-post")

  (+quail-defun "french-postfix" ";"
    (delete-horizontal-space)
    (insert " ; "))

  (+quail-defun "french-postfix" ":"
    (delete-horizontal-space)
    (let ((left-pad (cond
                     ((equal (char-before) ?:)
                      "")
                     ((and (derived-mode-p 'org-mode) (org-at-item-p) (not (org-at-item-description-p)))
                      " ")
                     (t
                      " "))))
      (insert left-pad ": "))))


;;; LSP

(add-hook! (c-ts-base-mode
            bash-ts-mode
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


;;; Theme
;;
;; See `+theme-settings' for general theme & face settings.

(setq
 doom-font                 (font-spec :family "Fira Code" :size 12)
 doom-variable-pitch-font  (font-spec :family "Helvetica Neue" :size 12))

(with-demoted-errors "Error enabling theme on startup: %S"
  (+theme-update))

(setq +indent-guides-enabled-modes
      '(yaml-mode yaml-ts-mode nxml-mode python-ts-mode))

(setq-hook! (dired-mode treemacs-mode)
  display-line-numbers nil)


;;; Files & Projects

(pushnew! vc-directory-exclusion-list
          "node_modules"
          "cdk.out"
          "target"
          ".direnv")

(pushnew! completion-ignored-extensions
          ".DS_Store"
          ".eln"
          ".drv"
          ".direnv/"
          ".git/")

(after! project
  (mapc #'project-remember-projects-under
        `("~/.config/" "~/src/" ,@(f-directories "~/src"))))
