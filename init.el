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
       (undo +tree)
       ediff-exts
       vc
       vc-exts

       :completion
       (corfu +icons +orderless)
       (vertico +icons)

       :checkers
       (spell +aspell +everywhere)
       (syntax +flymake)

       :term
       eshell
       eshell-exts

       :tools
       debugging
       (docker +lsp)
       direnv
       editorconfig
       (eval +overlay)
       lookup
       (lsp +eglot)
       lsp-exts
       nix
       (magit +forge)
       magit-exts
       make
       search
       (terraform +lsp)

       :lang
       ;; coq
       ;; (csharp +lsp)
       data
       emacs-lisp
       emacs-lisp-exts
       (elixir +lsp)
       ;; (graphql +lsp)
       (json +lsp)
       (java +lsp)
       (latex +lsp +fold)
       (lua +lsp)
       (nix +lsp)

       ;; (ocaml +lsp)
       (org +dragndrop +roam2)
       (org-exts
        +citations
        ;; +initial-buffers
        +modern
        +nursery
        +roam
        +slack)

       plantuml
       ;; haskell
       (rust +lsp)
       (sh +lsp)
       ;; (swift +lsp)
       (typescript +lsp)
       (yaml +lsp)
       (zig +lsp)

       :ui
       doom
       doom-exts
       harpoon
       hl-todo
       indent-guides
       leader
       treemacs
       (ligatures +fira)
       modeline
       ophints
       (vc-gutter +pretty)
       vi-tilde-fringe

       :private
       org
       )
