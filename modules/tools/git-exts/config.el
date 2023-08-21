;;; tools/git-exts/config.el -*- lexical-binding: t; -*-

(after! git-auto-commit-mode
  (setq gac-debounce-interval 10)
  (setq gac-silent-message-p t)
  (setq gac-automatically-push-p t)
  (setq gac-automatically-add-new-files-p t)
  (put 'gac-automatically-push-p 'safe-local-variable 'booleanp))

(after! magit
  (setq magit-save-repository-buffers 'dontask)
  (magit-todos-mode +1))

(after! magit-todos
  (setq magit-todos-ignored-keywords '("NOTE" "DONE" "KLUDGE" "HACK")))

(after! browse-at-remote
  (setq browse-at-remote-add-line-number-if-no-region-selected t))

(map! :after (:and magit evil-collection)
      :map magit-mode-map
      :n "z" nil)
