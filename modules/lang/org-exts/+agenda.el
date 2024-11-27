;;; lang/org-exts/+agenda.el -*- lexical-binding: t; -*-

;;; Global settings

(setq org-agenda-files (expand-file-name "org-agenda-files" org-directory))
(setq org-agenda-text-search-extra-files `(agenda-archives ,(expand-file-name "archive.org" org-directory)))
(setq org-agenda-search-view-always-boolean t)


;;; Configure my agenda views.

(setq org-agenda-custom-commands
      (let ((today '(agenda ""
                     ((org-agenda-overriding-header "Today")
                      (org-agenda-use-time-grid t)
                      (org-agenda-clockreport-parameter-plist '(:compact t
                                                                :link t
                                                                :maxlevel 3
                                                                :fileskip0 t
                                                                :filetitle t))
                      (org-agenda-skip-function #'+agenda-view-skip-function))))
            (next-actions '(tags-todo "-project-tickler-outline-inbox+TODO=\"TODO\""
                            ((org-agenda-overriding-header "Next Actions")
                             (org-agenda-skip-function #'+agenda-next-actions-skip-function))))

            (inbox '(tags-todo "+inbox+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Inbox"))))

            (delegated '(todo "WAIT"
                         ((org-agenda-overriding-header "Delegated")
                          (org-agenda-skip-function #'+agenda-delegated-section-skip-function))))

            (projects '(tags-todo "+TODO=\"TODO\"+project"
                        ((org-agenda-overriding-header "Projects"))))

            (tickler
             '(todo "TODO"
               ((org-agenda-overriding-header "Tickler")
                (org-agenda-skip-function #'+agenda-tickler-section-skip-function))))


            (unprocessed-notes
             '(tags-todo "+outline-project+TODO=\"TODO\""
               ((org-agenda-overriding-header "Unprocessed Notes")
                (org-agenda-skip-function #'+agenda-next-actions-skip-function))))

            (defaults `((org-agenda-todo-ignore-scheduled t)
                        (org-agenda-span 'day)
                        (org-agenda-window-setup 'only-window)
                        (org-agenda-start-day nil)
                        (org-agenda-include-diary nil)
                        (org-agenda-insert-diary-extract-time nil)
                        (org-agenda-show-inherited-tags nil)
                        (org-agenda-skip-deadline-if-done t)
                        (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
                        (org-agenda-skip-scheduled-if-done t)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-window-setup 'only-window)
                        (org-agenda-dim-blocked-tasks 'invisible)
                        (org-agenda-sorting-strategy '((agenda time-up category-up priority-down todo-state-up)
                                                       (todo priority-down category-up scheduled-up)
                                                       (tags priority-down category-up)
                                                       (search category-up)))
                        (org-agenda-clock-report-header "\nClocking")
                        (org-agenda-tags-column -100)
                        (org-agenda-use-time-grid nil)
                        (org-agenda-start-with-log-mode '(closed state))
                        (org-agenda-show-future-repeats nil)
                        (org-agenda-ignore-properties '(effort appt))
                        (org-agenda-archives-mode t))))

        `(("p" "personal agenda" ,(list today next-actions inbox delegated projects tickler)
           (,@defaults
            (org-agenda-tag-filter-preset '("-someday" "-ignore" "-work" "-outline"))))
          ("w" "work agenda" ,(list today next-actions inbox delegated projects tickler unprocessed-notes)
           (,@defaults
            (org-agenda-tag-filter-preset (list "-someday" "-ignore" (format "+%s" (timekeep-work-tag))))
            (org-agenda-clock-consistency-checks
             '(:gap-ok-around ("12:20" "12:40" "4:00")
               :max-duration "10:00"
               :min-duration 0
               :max-gap 0)))))))


;;; Configure org-super-agenda

(use-package! org-super-agenda
  :after org-agenda
  :if (modulep! +super-agenda)
  :config
  (org-super-agenda-mode +1))


;;; Configure keymaps

(after! evil
  ;; FIXME: Figure out why initial state is currently defaulting to `emacs'.
  (evil-set-initial-state 'org-agenda-mode 'motion))

(map! :map org-agenda-mode-map
      :m "/" 'org-agenda-filter
      :m "?" 'org-agenda-filter-by-tag
      :m "B" 'org-agenda-bulk-action
      :m "v" 'org-agenda-view-mode-dispatch
      :m "t" 'org-agenda-todo
      :m "J" 'org-agenda-goto-date
      :m "j" 'org-agenda-next-line
      :m "k" 'org-agenda-previous-line
      :m "f" 'org-agenda-later
      :m "b" 'org-agenda-earlier
      :m "M-j" 'org-agenda-next-item
      :m "M-k" 'org-agenda-previous-item
      :m "M-h" 'org-agenda-earlier
      :m "M-l" 'org-agenda-later
      :m "gd" 'org-agenda-toggle-time-grid
      :m "gr" 'org-agenda-redo
      :m "M-RET" 'org-agenda-show-and-scroll-up
      :m "C-f" 'evil-scroll-page-down
      :m "C-b" 'evil-scroll-page-up
      :m [remap save-buffer] 'org-save-all-org-buffers
      ;; Restore bindings for search buffers
      :m "+" 'org-agenda-manipulate-query-add
      :m "`" 'org-agenda-manipulate-query-add-re
      :m "-" 'org-agenda-manipulate-query-subtract
      :m "_" 'org-agenda-manipulate-query-subtract-re)




;;; Use page-break-lines to draw separator in org-agenda.

(setq org-agenda-block-separator (char-to-string ?\f))

(autoload 'page-break-lines--update-display-tables "page-break-lines")

(define-advice org-agenda (:after (&rest _) draw-separator)
  (page-break-lines--update-display-tables))

(define-advice org-agenda-redo (:after (&rest _) draw-separator)
  (page-break-lines--update-display-tables))



;;; Search for and update agenda files automatically

(defvar +org--agenda-update-process nil)

(defconst +agenda-files-update-script (file-name-concat (dir!) "update-agenda-files.sh"))

(defun +org-agenda-update-files ()
  (interactive)
  (unless (and +org--agenda-update-process (process-live-p +org--agenda-update-process))
    (setq +org--agenda-update-process
          (start-process "update-org-agenda-files" nil +agenda-files-update-script))))

(add-hook! 'org-mode-hook
  (add-hook! 'after-save-hook :local t
    (+org-agenda-update-files)))



;;; Forget deleted agenda files without prompting
(define-advice org-check-agenda-file (:override (file) always-remove-missing)
  (unless (file-exists-p file)
    (org-remove-file file)
    (throw 'nextfile t)))




;;; Reveal context around item on TAB
(add-hook! 'org-agenda-after-show-hook
  (org-overview)
  (org-reveal)
  (org-fold-show-subtree)
  (org-display-outline-path))
