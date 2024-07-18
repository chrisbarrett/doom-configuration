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

(when (modulep! +lsp)
  (add-hook 'java-ts-mode #'lsp!)
  (add-hook 'typescript-ts-mode-hook #'lsp!))

(after! smartparens
  ;; (|sys).path.append---the dot should not travel with the closing
  ;; paren
  (dolist (mode '(typescript-ts-mode js-ts-mode))
    (add-to-list 'sp-sexp-suffix (list mode 'regexp ""))))
