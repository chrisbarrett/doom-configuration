;;; app/chatgpt/config.el -*- lexical-binding: t; -*-

(after! chatgpt-shell
  (setq chatgpt-display-function #'display-buffer)
  (setq chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com")))

  (let ((gpt-latest "gpt-4-1106-preview" ))
    (setq chatgpt-shell-model-version gpt-latest)
    (add-to-list 'chatgpt-shell-model-versions gpt-latest 'append)))

(after! ob-chatgpt-shell
  (ob-chatgpt-shell-setup))
