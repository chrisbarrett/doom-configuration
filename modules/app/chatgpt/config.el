;;; app/chatgpt/config.el -*- lexical-binding: t; -*-

(after! chatgpt-shell
  (evil-set-initial-state 'chatgpt-shell-mode 'insert)
  (setq chatgpt-display-function #'display-buffer)
  (setq chatgpt-shell-openai-key
        (let ((cached))
          (lambda ()
            (let ((result (or cached
                              (string-trim (shell-command-to-string "op read 'op://Private/OpenAI API Key/token'")))))
              (when (string-match-p (rx bol "[ERROR]") result)
                (error "Error caching OpenAI API key: %s" result))
              (setq cached result)))))

  (let ((gpt-latest "gpt-4-1106-preview" ))
    (setq chatgpt-shell-model-version gpt-latest)
    (add-to-list 'chatgpt-shell-model-versions gpt-latest 'append)))

(after! ob-chatgpt-shell
  (ob-chatgpt-shell-setup))
