;; -*- lexical-binding: t; -*-

(map! :after hexl
      :map hexl-mode-map
      :n "]]" #'hexl-end-of-1k-page
      :n "[[" #'hexl-beginning-of-1k-page
      :n "h" #'hexl-backward-char
      :n "l" #'hexl-forward-char
      :n "j" #'hexl-next-line
      :n "k" #'hexl-previous-line
      :n "$" #'hexl-end-of-line
      :n "^" #'hexl-beginning-of-line
      :n "0" #'hexl-beginning-of-line)
