;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; Add extensions dir to the load-path
(after! corfu
  (let* ((corfu-path (locate-library "corfu"))
         (extensions-dir (file-name-concat (file-name-directory corfu-path)
                                           "extensions")))
    (add-to-list 'load-path extensions-dir)))

(after! corfu
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.3)
  (setq corfu-auto-prefix 3)

  (add-hook! minibuffer-setup-hook
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))

  (map! :after evil :map corfu-map
        [remap corfu-quit] (cmd! (corfu-quit)
                                 (when (evil-insert-state-p)
                                   (evil-normal-state)))))

(use-package! corfu-popupinfo :after corfu
  :init
  (setq-hook! 'emacs-lisp-mode-hook corfu-popupinfo-delay 0.3)
  :config
  (map! :map corfu-map
        "M-n" 'corfu-popupinfo-scroll-up
        "M-p" 'corfu-popupinfo-scroll-down
        "<f1>" 'corfu-popupinfo-toggle)
  (corfu-popupinfo-mode))

(add-hook 'doom-first-input-hook #'global-corfu-mode)

(after! kind-icon
  (setq corfu-margin-formatters '(kind-icon-margin-formatter))
  (setq kind-icon-use-icons nil)
  (setq kind-icon-default-face 'corfu-default))

(use-package! kind-icon :after corfu
  :config
  (add-hook! 'after-load-theme-functions
    (kind-icon-reset-cache)))
