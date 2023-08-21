;;; lang/org-exts/+citations.el -*- lexical-binding: t; -*-

(defvar +bibfiles nil)
(defvar +roam-litnotes-paths nil)

(after! org
  (require 'citar-org)
  (setq org-cite-global-bibliography +bibfiles)
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  (setq org-cite-csl-styles-dir (expand-file-name "~/Documents/Zotero/styles/")))

(map! :map org-mode-map
      :after org
      :prefix "C-c"
      :desc "Insert citation" "@" 'org-cite-insert
      :desc "Insert citation (citar)" "b" #'citar-insert-citation)

(after! citar
  (setq citar-bibliography +bibfiles)
  (setq citar-display-transform-functions `((("author" "editor") . ,(-compose 'citar--shorten-names '+citations-clean-bibtex-string))
                                            (("title") . +citations-clean-bibtex-string)))
  (setq citar-notes-paths +roam-litnotes-paths)
  (setq citar-open-note-function '+citations-go-to-litnote-for-citekey)
  (setq citar-symbol-separator "  ")

  (add-hook 'org-mode-hook 'cursor-sensor-mode))

(after! citar-org
  (add-to-list 'citar-org-activation-functions #'+org-cite-collapse-activation-function t))

(after! (:and citar all-the-icons)
  (setf (citar-indicator-symbol citar-indicator-files) (all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1))
  (setf (citar-indicator-symbol citar-indicator-notes) (all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3))
  (setf (citar-indicator-symbol citar-indicator-links) (all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01)))

(when (modulep! +roam)
  (after! citar
    (citar-org-roam-mode))

  (after! citar-org-roam
    (setf (plist-get citar-org-roam-notes-config :create) #'+citations-go-to-litnote-for-citekey)))
