;; -*- lexical-binding: t; -*-

(after! ispell
  (setq ispell-dictionary "en_GB")
  (setq ispell-silently-savep t))

(map! :n "z SPC" #'ispell-word)
