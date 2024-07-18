;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(require 'pcase)
(require 'map)


;; KLUDGE: Enable undo-tree manually; no idea why this isn't being applied
;; correctly by doom.
(autoload 'turn-on-undo-tree-mode "undo-tree")
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)

;;; Eglot (LSP)

(when (modulep! :tools lsp +eglot)
  (add-hook! (nix-mode bash-ts-mode java-ts-mode rustic-mode rust-ts-mode) #'eglot-ensure)

  (defun +eglot-run-organise-imports ()
    (when (derived-mode-p 'java-ts-mode)
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'eglot-code-action-organize-imports))))

  (defun +configure-eglot-formatting ()
    (cond
     ((eglot-managed-p)
      (add-hook 'before-save-hook #'+eglot-run-organise-imports nil t)
      (add-hook 'before-save-hook #'eglot-format nil t))
     (t
      (remove-hook 'before-save-hook #'+eglot-run-organise-imports t)
      (remove-hook 'before-save-hook #'eglot-format t))))

  (add-hook 'eglot-managed-mode-hook #'+configure-eglot-formatting))

;;; Themeing

(setq doom-font (font-spec :family "Fira Code" :size 12))
(setq doom-variable-pitch-font (font-spec :family "Helvetica Neue" :size 12))

(condition-case err
    (setq doom-theme (+theme-for-system-theme))
  (error (warn "Failed to set theme on startup: %s " (error-message-string err))))

(setq display-line-numbers-type 'relative)
(setq-hook! dired-mode display-line-numbers nil)
(setq-hook! treemacs-mode display-line-numbers nil)

;;; Indent-guides

(remove-hook! (prog-mode text-mode conf-mode) 'highlight-indent-guides-mode)
(add-hook! (yaml-mode yaml-ts-mode nxml-mode python-ts-mode) 'highlight-indent-guides-mode)

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-responsive t))

;;; Window management

(map! "C-SPC" #'consult-buffer)

(after! window
  (setq switch-to-buffer-obey-display-actions t)
  (setq switch-to-buffer-in-dedicated-window 'pop))

(after! help
  (setq help-window-select t))

(map! :after winner
      :n "C-." nil)

(map! :map winner-mode-map
      :after winner
      :gvni "C-," #'winner-undo
      :gvni "C-." #'winner-redo)

;;; Projects

(after! project
  (project-remember-projects-under "~/.config/")
  (project-remember-projects-under "~/src/")
  (dolist (dir (seq-difference (directory-files "~/src/") '("." "..")))
    (when (file-directory-p dir)
      (project-remember-projects-under dir))))

;;; Org-Mode

(setq org-directory "~/org")
(setq org-roam-directory (file-name-concat org-directory "roam"))

(setq +bibfiles (list (file-name-concat org-directory "org-roam.bib")))
(setq +roam-litnotes-paths (list (file-name-concat org-roam-directory "litnotes")))

(setq +roam-index-node-id "0F0670F7-A280-4DD5-8FAC-1DB3D38CD37F")

;;; Flymake

(map! :map 'flymake-mode-map
      "M-n" #'flymake-goto-next-error
      "M-p" #'flymake-goto-prev-error)

;;; Spelling

(after! ispell
  (setq ispell-dictionary "en_GB")
  (setq ispell-silently-savep t))

(map! :n "z SPC" #'ispell-word)

;;; Completion

;; Hide commands irrelevant to current mode from M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)

;; Perform both indentation & text completion with TAB.
(setq tab-always-indent 'complete)

(dolist (entry '(".DS_Store" ".eln" ".drv" ".direnv/" ".git/"))
  (add-to-list 'completion-ignored-extensions entry))

(map! :after vertico
      :map vertico-map
      "C-k" #'kill-line
      "C-<return>" 'vertico-exit-input
      "M-<return>" 'minibuffer-force-complete-and-exit)

;;; Info & Help

(map! :map Info-mode-map
      :after info
      :n "M-," #'Info-history-back
      :n "M-." #'Info-history-forward
      :n "^"   #'Info-up
      :n "C-n" #'Info-forward-node
      :n "C-p" #'Info-backward-node
      :n ">" #'Info-next
      :n "<" #'Info-prev
      :n "]" #'Info-next-reference
      :n "[" #'Info-prev-reference
      :n "H"   #'Info-top-node
      :n "~"   #'Info-directory
      [remap consult-imenu] #'Info-toc)

;;; Tree-sitter
;;
;; Remap major modes to use treesit modes, including in org-mode.

(defconst +treesit-mode-remaps
  '((:orig-mode c-mode :treesit-mode c-ts-mode :org-src ("C"))
    (:orig-mode c++-mode :treesit-mode c++-ts-mode :org-src ("cpp" "c++"))
    (:orig-mode c-or-c++-mode :treesit-mode c-or-c++-ts-mode)
    (:orig-mode conf-toml-mode :treesit-mode toml-ts-mode :org-src ("conf-toml" "toml"))
    (:orig-mode csharp-mode :treesit-mode csharp-ts-mode :org-src ("csharp"))
    (:orig-mode dockerfile-mode :treesit-mode dockerfile-ts-mode :org-src ("dockerfile"))
    (:orig-mode java-mode :treesit-mode java-ts-mode :org-src ("java"))
    (:orig-mode js-mode :treesit-mode js-ts-mode :org-src "js")
    (:orig-mode json-mode :treesit-mode json-ts-mode :org-src "json")
    (:orig-mode python-mode :treesit-mode python-ts-mode :org-src ("python" "py"))
    (:orig-mode rust-mode :treesit-mode rust-ts-mode :org-src ("rust"))
    (:orig-mode sh-mode :treesit-mode bash-ts-mode)
    (:orig-mode toml-mode :treesit-mode toml-ts-mode :org-src ("toml"))
    (:orig-mode typescript-mode :treesit-mode typescript-ts-mode :org-src ("ts" "typescript"))
    (:orig-mode yaml-mode :treesit-mode yaml-ts-mode :org-src ("yml" "yaml"))))

(mapc (pcase-lambda ((map :orig-mode :treesit-mode :org-src))
        (after! files
          (add-to-list 'major-mode-remap-alist (cons orig-mode treesit-mode)))
        (after! org-src
          (dolist (src-type (-list org-src))
            (let ((treesit-sans-suffix (intern (string-remove-suffix "-mode" (format "%s" treesit-mode)))))
              (setf (alist-get src-type org-src-lang-modes) treesit-sans-suffix)))))
      +treesit-mode-remaps)

;; The ES modules spec provides for more extensions that are mirrored in
;; TypeScript.
(add-to-list 'auto-mode-alist
             (cons (rx "." (? (any "c" "m") "ts") eos)
                   'typescript-ts-mode))

;; `bash-ts-mode' falls back to sh-mode, so we don't want to use
;; major-mode-remap-alist to set it.

(setq auto-mode-alist
      (seq-map (pcase-lambda (`(,pat . ,mode))
                 (cons pat (if (equal mode 'sh-mode)
                               'bash-ts-mode
                             mode)))
               auto-mode-alist))

;;; Oils for Unix
;;
;; I eagerly await a real major mode, if this scripting language takes off.

(add-to-list 'auto-mode-alist (cons (rx ".oil" eos) 'prog-mode))
(add-to-list 'magic-mode-alist (cons (rx bol "#!/usr/bin/env" (+ space) "oil" symbol-end) 'prog-mode))

;;; Strings

(map! "M-s" #'string-inflection-all-cycle
      "M-f" #'unfill-toggle)

;;; Rust

(after! (:and rustic-babel ob)
  (setq rustic-babel-default-toolchain "nightly"))

(after! eglot
  (setf (alist-get '(rust-mode rust-ts-mode) eglot-server-programs nil nil #'equal)
        '("rust-analyzer" :initializationOptions (:checkOnSave (:command "clippy")))))


;;; Shell-scripting

(after! sh-script
  (setq sh-basic-offset 2))

(add-to-list 'auto-mode-alist (cons (rx "/.envrc") 'bash-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx "sshd_config" eol) 'conf-unix-mode))
(add-to-list 'auto-mode-alist (cons (rx "/sshd_config.d/") 'conf-unix-mode))

;;; Java - woe is me

(after! eglot
  (setf (alist-get '(java-mode java-ts-mode) eglot-server-programs nil nil #'equal)
        (cons "jdtls"
              (seq-map (lambda (s) (concat "--jvm-arg=" s))
                       (list "-XX:+UseParallelGC"
                             "-XX:GCTimeRatio=4"
                             "-XX:AdaptiveSizePolicyWeight=90"
                             "-Dsun.zip.disableMemoryMapping=true"
                             "-Dlog.protocol=true"
                             "-Dlog.level=ALL"
                             "-Xmx1G"
                             "-Xms100m"
                             (concat "-javaagent:" (getenv "NIX_EMACS_LOMBOK_JAR")))))))

;;; Env files

(add-to-list 'auto-mode-alist (cons (rx "/.env" (? "." (+ nonl)) eol) 'conf-unix-mode))

;;; ASM

(after! asm-mode
  (setq-hook! asm-mode tab-width 8)
  (add-hook! asm-mode
    (indent-tabs-mode +1)))

(map! :gniv "C-t" 'project-find-file
      :gniv "C-/" '+vertico/project-search)


;;; Projects

(after! vc-hooks
  (add-to-list 'vc-directory-exclusion-list ".direnv")
  (add-to-list 'vc-directory-exclusion-list "target")
  (add-to-list 'vc-directory-exclusion-list "cdk.out")
  (add-to-list 'vc-directory-exclusion-list "node_modules"))


;;; Treemacs

(after! treemacs
  (setq treemacs-collapse-dirs 8)
  ;; (add-hook 'treemacs-mode-hook #'treemacs-hide-gitignored-files-mode)
  ;; TODO: Reset to t once I have a better window management system.
  (setq treemacs-is-never-other-window nil)

  (defun config-treemacs--extra-ignore-file-predicate (file abspath)

    (or (seq-contains-p '(".direnv"
                          ".stfolder"
                          ".cdk.out"
                          "target"
                          "dist"
                          "out")
                        file)
        (string-match-p (rx bol ".stignore") file)))

  (add-to-list 'treemacs-ignored-file-predicates #'config-treemacs--extra-ignore-file-predicate))
