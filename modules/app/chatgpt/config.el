;;; app/chatgpt/config.el -*- lexical-binding: t; -*-

(after! chatgpt-shell
  (setq chatgpt-display-function #'display-buffer)
  (setq chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))

(after! ob-chatgpt-shell
  (ob-chatgpt-shell-setup))
