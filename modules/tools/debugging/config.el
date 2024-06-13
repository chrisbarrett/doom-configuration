;;; lang/debugging/config.el -*- lexical-binding: t; -*-

(after! dape
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-cwd-fn #'project-root)
  (map! :map prog-mode-map :niv (kbd "M-d") dape-global-map)
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t))))
