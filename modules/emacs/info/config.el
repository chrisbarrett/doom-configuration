;; -*- lexical-binding: t; -*-

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
