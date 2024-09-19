;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; See: https://github.com/radian-software/straight.el#the-recipe-format

(package! lorem-ipsum)

(package! company-sourcekit :disable t)
(package! flycheck-plantuml :disable t)
(package! flymake-popon :disable t)
(package! lsp-mode :disable t)
(package! lsp-sourcekit :disable t)
(package! smartparens-python :disable t)
(package! string-inflection)
(package! unfill)

(package! eglot-booster
  :recipe
  (:type git
   :host github
   :repo "jdtsmith/eglot-booster"))

(package! org :built-in t)
