;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' to access Doom's documentation.

(setq doom-modules-dirs
      (list (file-name-concat doom-user-dir "modules/")
            "~/org/modules/"
            doom-modules-dir))

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' to
;;      view its documentation. This works on flags as well (those symbols that
;;      start with a plus).
;;
;;      Alternatively, press 'gd' on a module to browse its directory (for easy
;;      access to its source code).

(doom! :compat
       (:if (getenv "EMACS_IN_NIX") nix)
       :os
       (:if IS-MAC macos)

       :config
       (default +smartparens) ; Doom's built-in defaults
       default-exts

       :editor
       (evil +everywhere)
       evil-exts
       extra-cmds
       file-templates
       (format +onsave)
       fold
       linewise
       multiple-cursors
       rotate-text
       smartparens-exts
       snippets
       snippets-exts
       word-wrap

       :emacs
       (dired +dirvish)
       dired-exts
       (ibuffer +icons)
       info
       (undo +tree)
       ediff-exts
       treesit
       vc
       vc-exts

       :completion
       corfu
       defaults
       (vertico +icons)
       vertico-exts

       :checkers
       (spell +aspell +everywhere)
       spell-exts
       (syntax +flymake)
       syntax-exts

       :term
       eshell
       eshell-exts

       :tools
       (docker +lsp)
       direnv
       editorconfig
       (eval +overlay)
       git-exts
       lookup
       (lsp +eglot)
       lsp-exts
       nix
       magit
       make
       search
       strings
       terraform

       :app
       chatgpt

       :lang
       coq
       (csharp +lsp)
       data
       emacs-lisp
       emacs-lisp-exts
       (graphql +lsp)
       (json +lsp)
       (latex +lsp +fold)
       nix

       (org +dragndrop +roam2)
       (org-exts
        +citations
        ;; +initial-buffers
        +modern
        +nursery
        +roam
        +slack)

       plantuml
       (rust +lsp)
       rust-exts
       (sh +lsp)
       sh-exts
       (swift +lsp)
       (typescript +lsp)
       (yaml +lsp)
       (zig +lsp)

       :ui
       doom
       doom-exts
       (emoji +github +unicode)
       emoji-exts
       hl-todo
       indent-guides
       indent-guides-exts
       leader
       (ligatures +fira)
       modeline
       ophints
       (popup +defaults +all)
       (vc-gutter +pretty)
       vi-tilde-fringe
       window-management

       :private
       org
       )
