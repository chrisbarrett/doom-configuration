;; -*- lexical-binding: t; -*-

(map! :map deadgrep-mode-map
      :after deadgrep
      :desc "Edit results" "C-c C-e" #'deadgrep-edit-mode)

(map! :map deadgrep-mode-map
      :after deadgrep
      :n "gr" 'deadgrep-restart)

(map! :map deadgrep-edit-mode-map
      :after deadgrep
      :desc "Save results" "C-c C-c" #'deadgrep-mode
      :desc "Save results" "C-x C-s" #'deadgrep-mode)

(after! deadgrep
  (setq-default deadgrep--search-type 'regexp))

(after! consult
  (setq consult-ripgrep-args '("rg"
                               "--follow"
                               "--null"
                               "--line-buffered"
                               "--color=never"
                               "--max-columns=1000"
                               "--path-separator /"
                               "--smart-case"
                               "--no-heading"
                               "--with-filename"
                               "--line-number"
                               "--search-zip")))
