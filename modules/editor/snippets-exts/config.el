;;; editor/snippets-exts/config.el -*- lexical-binding: t; -*-

;; I sometimes want to do just-one-space within a snippet definition. There's no
;; nice way to do this. The hacky solution is to add an inline lisp call inline
;; to `just-one-space'.

(after! warnings
  (add-to-list 'warning-suppress-types '(yasnippet))
  (add-to-list 'warning-suppress-log-types '(yasnippet)))

(set-file-template! (rx "flake.nix" eos) :trigger "flake")

(map! :map yas-keymap
      :after yasnippet
      "SPC" (general-predicate-dispatch 'self-insert-command
              (yas--maybe-clear-field-filter t) 'yas-skip-and-clear-field)
      "<backspace>"
      (general-predicate-dispatch 'backward-delete-char
        (yas--maybe-clear-field-filter t) 'yas-skip-and-clear-field
        (bound-and-true-p smartparens-mode) 'sp-backward-delete-char))

(defadvice! +yas-goto-field-end (&rest _)
  "Place point at the end of previous field when cycling backwards."
  :after '(yas-next-field yas-prev-field)
  (when-let* ((field (yas-current-field)))
    (when (and (yas--field-modified-p field)
               (yas--field-contains-point-p field))
      (goto-char (marker-position (yas--field-end field)))))
  (when (and (boundp 'evil-mode) evil-mode (fboundp 'evil-insert-state))
    (evil-insert-state)))
