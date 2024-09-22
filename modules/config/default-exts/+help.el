;;; config/default-exts/+help.el -*- lexical-binding: t; -*-

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
