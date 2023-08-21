;; -*- lexical-binding: t; -*-

(after! eshell
  (setq eshell-prefer-lisp-functions nil)
  (setq eshell-prefer-lisp-variables t))

(map! :map eshell-mode-map
      :after eshell
      :in "C-n"      #'eshell-next-matching-input-from-input
      :in "C-p"      #'eshell-previous-matching-input-from-input
      :in "C-a"      #'eshell-bol
      :in "C-e"      #'end-of-line
      :in "C-c C-l"  (cmd! (eshell/clear-scrollback)
                           (eshell-emit-prompt))
      :v "s"         #'evil-surround-region)
