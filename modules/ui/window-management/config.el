;; -*- lexical-binding: t; -*-

(after! window
  (setq switch-to-buffer-obey-display-actions t)
  (setq switch-to-buffer-in-dedicated-window 'pop))

(after! help
  (setq help-window-select t))

(map! :after winner
      :n "C-." nil)

(map! :map winner-mode-map
      :after winner
      :gvni "C-," #'winner-undo
      :gvni "C-." #'winner-redo)
