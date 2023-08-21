;;; lang/typescript/config.el -*- lexical-binding: t; -*-

(add-hook! find-file-hook
  (when (string-match-p  "/node_modules/" default-directory)
    (read-only-mode +1)))

(add-hook! '(js-base-mode-hook typescript-ts-base-mode-hook)
           :depth 10
           #'prettier-mode)

(add-to-list 'auto-mode-alist (cons (rx "." (? (any "m" "c")) "ts" (? "x") eos)
                                    'typescript-ts-mode))

(setq-hook! 'typescript-ts-base-mode-hook +evil-want-o/O-to-continue-comments nil)

(when (getenv "EMACS_IN_NIX")
  (define-advice prettier--find-node (:around (fn server-id) inject-via-envvar)
    (pcase server-id
      ('local
       (getenv "NIX_EMACS_NODE_PROGRAM"))
      (_
       (funcall fn server-id)))))

(when (modulep! +lsp)
  (add-hook 'typescript-ts-mode-hook #'lsp!))

(after! smartparens
  ;; (|sys).path.append---the dot should not travel with the closing
  ;; paren
  (dolist (mode '(typescript-ts-mode js-ts-mode))
    (add-to-list 'sp-sexp-suffix (list mode 'regexp ""))))

(after! prettier
  (setq prettier-inline-errors-flag t))
