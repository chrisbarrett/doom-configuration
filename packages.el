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

;; Pin until doom updates its version
(package! magit-todos :pin "d0646dbbf46d75d08e3d7b4c665d7d763a468af1")
