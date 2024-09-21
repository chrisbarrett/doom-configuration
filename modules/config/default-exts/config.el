;; -*- lexical-binding: t; -*-

(require 'pcase)
(require 'map)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(after! password-cache
  (setq password-cache t)
  (setq password-cache-expiry 300))

(setq save-interprogram-paste-before-kill t)


;;; Set some handy global keybindings

(map! :g "M-t" #'transpose-words
      :g "M-SPC" #'cycle-spacing
      :g "C-x a a" #'align-regexp)


;;; Enable useful commands that are disabled by default

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


;;; Make narrow-to-region exit visual state

(when (modulep! :editor evil)
  (define-advice narrow-to-region (:after (&rest _) back-to-normal-state)
    (when (and (equal evil-state 'visual)
               (called-interactively-p 'any))
      (evil-normal-state)
      (goto-char (point-min)))))


;;; Use drag-stuff for line transposition

(after! drag-stuff
  (setq drag-stuff-modifier 'control))

(ignore-errors
  (add-transient-hook! doom-first-input-hook
    #'drag-stuff-global-mode))

(map! :map drag-stuff-mode-map
      :ni "C-<up>" #'drag-stuff-up
      :ni "C-<down>" #'drag-stuff-down)


;;; Navigating to definitions

(after! xref
  (setq xref-auto-jump-to-first-definition t)
  (setq xref-auto-jump-to-first-xref 'show))

;; KLUDGE: Key bindings aren't applied when specifying states via keywords, but
;; specifying the keymap manually works.
(map! :after evil
      :map evil-normal-state-map
      "M-." (if (modulep! :tools lookup) #'+lookup/definition #'xref-find-definitions)
      "gm" (if (modulep! :tools lookup) #'+lookup/references #'xref-find-references))

(when IS-MAC
  (load! "+macos"))


;;; Standardise help buffer keybindings

(map!
 (:after help :map help-mode-map
  :n "o"       #'link-hint-open-link)

 (:after helpful :map helpful-mode-map
  :n "o"       #'link-hint-open-link)

 (:after info :map Info-mode-map
  :n "o"       #'link-hint-open-link
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

 (:after apropos :map apropos-mode-map
  :n "o"       #'link-hint-open-link
  :n "TAB"     #'forward-button
  :n [tab]     #'forward-button
  :n [backtab] #'backward-button)

 (:after view :map view-mode-map
         [escape]  #'View-quit-all)

 (:after man :map Man-mode-map
  :n "q"    #'kill-current-buffer)

 (:after geiser-doc :map geiser-doc-mode-map
  :n "o"    #'link-hint-open-link))



;;; Silence warnings when helpful is looking up read-only files.

;; See: https://github.com/Wilfred/helpful/issues/338

(after! helpful
  (define-advice helpful--open-if-needed (:around (fn &rest args) suppress-warnings)
    (let ((noninteractive t))
      (apply fn args))))


;;; Deal with inconsistent tabs vs spaces in Emacs C srcs

(defun +config-apply-emacs-C-src-file-settings ()
  (setq-local tab-width 8)
  (whitespace-mode -1))

(defconst +emacs-C-src-file-regexp
  (rx "/share/emacs/" (+ digit) "." (+ digit) "/src/"))

(add-hook! 'c-ts-mode-hook
  (when (and (buffer-file-name)
             (string-match-p +emacs-C-src-file-regexp (buffer-file-name)))
    (+config-apply-emacs-C-src-file-settings)))


;;; Completion

;; Hide commands irrelevant to current mode from M-x
(setq read-extended-command-predicate #'command-completion-default-include-p)

(setq completion-ignore-case t)
(setq completion-cycle-threshold 3)

;; Perform both indentation & text completion with TAB.
(setq tab-always-indent 'complete)

(dolist (entry '(".DS_Store" ".eln" ".drv" ".direnv/" ".git/"))
  (add-to-list 'completion-ignored-extensions entry))


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

;; `bash-ts-mode' falls back to sh-mode, so we don't want to use
;; major-mode-remap-alist to set it.

(setq auto-mode-alist
      (seq-map (pcase-lambda (`(,pat . ,mode))
                 (cons pat (if (equal mode 'sh-mode)
                               'bash-ts-mode
                             mode)))
               auto-mode-alist))

;;; ASM

(after! asm-mode
  (setq-hook! asm-mode tab-width 8)
  (add-hook! asm-mode
    (indent-tabs-mode +1)))

;;; Ispell

(after! ispell
  (setq ispell-silently-savep t))
