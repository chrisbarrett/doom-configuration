;; -*- lexical-binding: t; -*-
;
(require 'map)

(defconst +treesit-mode-remaps
  '((:orig-mode sh-mode :treesit-mode bash-ts-mode)
    (:orig-mode conf-toml-mode :treesit-mode toml-ts-mode :org-src ("conf-toml" "toml"))
    (:orig-mode csharp-mode :treesit-mode csharp-ts-mode :org-src ("csharp"))
    (:orig-mode dockerfile-mode :treesit-mode dockerfile-ts-mode :org-src ("dockerfile"))
    (:orig-mode js-mode :treesit-mode js-ts-mode :org-src "js")
    (:orig-mode json-mode :treesit-mode json-ts-mode :org-src "json")
    (:orig-mode python-mode :treesit-mode python-ts-mode :org-src ("python" "py"))
    (:orig-mode typescript-mode :treesit-mode typescript-ts-mode :org-src ("ts" "typescript"))
    (:orig-mode yaml-mode :treesit-mode yaml-ts-mode :org-src ("yml" "yaml"))))

;;; Remap major modes to use treesit modes, including in org-mode.

(pcase-dolist ((map :orig-mode :treesit-mode :org-src) +treesit-mode-remaps)
 (after! files
   (add-to-list 'major-mode-remap-alist (cons orig-mode treesit-mode)))
 (after! org-src
   (dolist (src-type (-list org-src))
     (let ((treesit-sans-suffix (intern (string-remove-suffix "-mode" (format "%s" treesit-mode)))))
       (setf (alist-get src-type org-src-lang-modes) treesit-sans-suffix)))))

(after! files
  ;; The ES modules spec provides for more extensions that are mirrored in
  ;; TypeScript.

  (add-to-list 'auto-mode-alist
               (cons (rx "." (? (any "c" "m") "ts") eos)
                     'typescript-ts-mode))

  ;; `bash-ts-mode' falls back to sh-mode, so we don't want to use
  ;; major-mode-remap-alist to set it.

  (setq auto-mode-alist
        (seq-map (pcase-lambda (`(,pat . ,mode))
                   (cons pat (if (equal mode 'sh-mode)
                                 'bash-ts-mode
                               mode)))
                 auto-mode-alist)))
