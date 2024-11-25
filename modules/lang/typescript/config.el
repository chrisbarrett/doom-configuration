;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(add-hook! find-file-hook
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
  (add-to-list 'eglot-server-programs '((js-mode js-ts-mode typescript-mode typescript-ts-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
          :lint t)))
