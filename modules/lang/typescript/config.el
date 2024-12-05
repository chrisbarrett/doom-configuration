;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(add-hook! 'find-file-hook
  (when (string-match-p  "/node_modules/" default-directory)
    (read-only-mode +1)))

(add-to-list 'auto-mode-alist (cons (rx "." (? (any "m" "c")) "ts" (? "x") eos)
                                    'typescript-ts-mode))

(add-to-list 'auto-mode-alist (cons (rx "." (? (any "m" "c")) "js" (? "x") eos)
                                    'js-ts-mode))

(add-to-list 'auto-mode-alist (cons (rx ".json"  eos) 'json-ts-mode))

(setq-hook! 'typescript-ts-base-mode-hook +evil-want-o/O-to-continue-comments nil)

(after! smartparens
  ;; (|sys).path.append---the dot should not travel with the closing
  ;; paren
  (dolist (mode '(typescript-ts-mode js-ts-mode))
    (add-to-list 'sp-sexp-suffix (list mode 'regexp ""))))

;; The ES modules spec provides for more extensions that are mirrored in
;; TypeScript.
(add-to-list 'auto-mode-alist
             (cons (rx "." (? (any "c" "m") "ts") eos)
                   'typescript-ts-mode))


;; Disable prettify-symbols for JS

(after! js
  (setq js--prettify-symbols-alist nil))

;; https://docs.deno.com/runtime/getting_started/setup_your_environment/#eglot
(after! eglot
  (let ((langs '((js-mode :language-id "javascript")
                 (js-ts-mode :language-id "javascript")
                 (tsx-ts-mode :language-id "typescriptreact")
                 (typescript-ts-mode :language-id "typescript")
                 (typescript-mode :language-id "typescript"))))
    (add-to-list 'eglot-server-programs `(,langs . ,(eglot-alternatives
                                                     '(("deno" "lsp" :initializationOptions (:enable t :lint t))
                                                       ("typescript-language-server" "--stdio")))))))
