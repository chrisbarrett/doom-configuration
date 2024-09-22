;; -*- lexical-binding: t; -*-

(load! "+treesit")
(load! "+help")

(when IS-MAC
  (load! "+macos"))

(require 'pcase)
(require 'map)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(after! password-cache
  (setq password-cache t)
  (setq password-cache-expiry 300))

(setq save-interprogram-paste-before-kill t)

(setq envrc-show-summary-in-minibuffer nil)


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

;;; ASM

(after! asm-mode
  (setq-hook! asm-mode tab-width 8)
  (add-hook! asm-mode
    (indent-tabs-mode +1)))

;;; Ispell

(after! ispell
  (setq ispell-silently-savep t))
