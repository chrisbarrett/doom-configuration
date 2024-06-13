;; -*- lexical-binding: t; -*-

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
  :n "o"       #'link-hint-open-link)
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
  :n "o"    #'link-hint-open-link)
 )

;;; Silence warnings when helpful is looking up read-only files.

;; See: https://github.com/Wilfred/helpful/issues/338

(after! helpful
  (define-advice helpful--open-if-needed (:around (fn &rest args) suppress-warnings)
    (let ((noninteractive t))
      (apply fn args))))
