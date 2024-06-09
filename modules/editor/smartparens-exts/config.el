;;; editor/smartparens-exts/config.el -*- lexical-binding: t; -*-

(after! smartparens
  (setq sp-navigate-close-if-unbalanced t)

  (map! :map smartparens-strict-mode-map
        :n "D" #'sp-kill-hybrid-sexp
        :in "M-'" (cmd! (sp-wrap-with-pair "'")))

  (sp-pair "`" "`"
           :bind "M-`")
  (sp-pair "{" "}"
           :bind "M-{"
           :pre-handlers '(+sp/add-space-before-sexp-insertion)
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "[" "]"
           :bind "M-["
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "(" ")"
           :bind "M-("
           :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (sp-pair "\"" "\""
           :bind "M-\""
           :pre-handlers '(:add (+sp/add-space-before-sexp-insertion)))

  (sp-with-modes (cons 'lisp-data-mode sp-lisp-modes)
    (sp-local-pair "(" nil
                   :pre-handlers '(+sp/add-space-before-sexp-insertion)
                   :post-handlers '(+sp/add-space-after-sexp-insertion))
    (sp-local-pair "[" nil
                   :pre-handlers '(+sp/add-space-before-sexp-insertion)
                   :post-handlers '(+sp/add-space-after-sexp-insertion))
    (sp-local-pair "\"" nil
                   :pre-handlers '(+sp/add-space-before-sexp-insertion)
                   :post-handlers '(+sp/add-space-after-sexp-insertion))
    (sp-local-pair "{" nil
                   :pre-handlers '(+sp/add-space-before-sexp-insertion)
                   :post-handlers '(+sp/add-space-after-sexp-insertion)))

  (sp-with-modes '(typescript-ts-mode)
    (sp-local-pair "<" ">" :actions nil))

  (sp-with-modes '(org-mode markdown-mode gfm-mode latex-mode)
    ;; Don't pad curly-braces.
    (sp-local-pair "{" "}" :pre-handlers nil))

  (sp-with-modes '(org-mode markdown-mode gfm-mode)
    (sp-local-pair "[" "]" :post-handlers '(+sp/format-checkitem)))

  (smartparens-global-strict-mode +1))
