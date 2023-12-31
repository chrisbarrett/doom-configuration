;;; lang/org-exts/+agenda.el -*- lexical-binding: t; -*-

;;; Global settings

(setq org-agenda-files (expand-file-name "org-agenda-files" org-directory))
(setq org-agenda-text-search-extra-files `(agenda-archives ,(expand-file-name "archive.org" org-directory)))
(setq org-agenda-search-view-always-boolean t)

;;; Configure my agenda views.

;; The todos shown in the agenda are aggressively filtered using skip functions, so
;; that subtasks are preferred. This is used to model GTD-style /next actions/.
;; Filtering can be suppressed for a specific tree by setting the `AGENDA_SKIP'
;; property to `ignore' or `scheduled'.

(setq org-agenda-custom-commands
      (let ((todos '(tags-todo "-bills-project-outline+TODO=\"TODO\""
                     ((org-agenda-overriding-header "Next Actions")
                      (org-agenda-skip-function #'+agenda-skip-items-already-shown))))
            (bills '(tags-todo "+TODO=\"TODO\"+bills"
                     ((org-agenda-overriding-header "Bills")
                      (org-agenda-skip-function #'+agenda-skip-items-already-shown))))
            (projects '(tags-todo "+TODO=\"TODO\"+project"
                        ((org-agenda-overriding-header "Projects"))))
            (delegated '(todo "WAIT"
                         ((org-agenda-overriding-header "Delegated")
                          (org-agenda-skip-function #'+agenda-skip-item-if-timestamp))))
            (today '(agenda ""

                     ((org-agenda-start-day "-3d")
                      (org-agenda-span 'week)
                      (org-agenda-overriding-header "Today")
                      (org-agenda-clockreport-parameter-plist '(:compact t
                                                                :link t
                                                                :maxlevel 3
                                                                :fileskip0 t
                                                                :filetitle t))
                      (org-agenda-use-time-grid t))))
            (notes
             '(tags-todo "-bills+outline-project+TODO=\"TODO\""
               ((org-agenda-overriding-header "Unprocessed Notes")
                (org-agenda-skip-function #'+agenda-skip-items-already-shown))))

            (defaults `(
                        ;; (org-stuck-projects '("" nil nil ""))
                        (org-agenda-todo-ignore-scheduled t)
                        (org-agenda-include-diary nil)
                        (org-agenda-insert-diary-extract-time nil)
                        (org-agenda-show-all-dates nil)
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

        `(("p" "personal agenda" ,(list todos bills delegated today)
           (,@defaults
            (org-agenda-tag-filter-preset '("-someday" "-ignore" "-work" "-outline"))))
          ("w" "work agenda" ,(list todos delegated projects today notes)
           (,@defaults
            (org-agenda-clockreport-mode t)
            (org-agenda-tag-filter-preset (list "-someday" "-ignore" (format "+%s" (timekeep-work-tag))))
            (org-agenda-clock-consistency-checks
             '(:gap-ok-around ("12:20" "12:40" "4:00")
               :max-duration "10:00"
               :min-duration 0
               :max-gap 0)))))))

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

;; Automatically remove deleted files from agenda
(define-advice org-check-agenda-file (:override (file) always-remove-missing)
  (unless (file-exists-p file)
    (org-remove-file file)
    (throw 'nextfile t)))

;;; Use page-break-lines to draw separator in org-agenda.

(setq org-agenda-block-separator (char-to-string ?\f))

(define-advice org-agenda (:after (&rest _) draw-separator)
  (page-break-lines--update-display-tables))

(define-advice org-agenda-redo (:after (&rest _) draw-separator)
  (page-break-lines--update-display-tables))

;;; Search for and update agenda files automatically

(defvar +org--agenda-update-process nil)

(defun +org-agenda-update-ids ()
  (interactive)
  (unless (and +org--agenda-update-process (process-live-p +org--agenda-update-process))
    (let ((default-directory org-directory))
      (setq +org--agenda-update-process
            (start-process-shell-command "update-org-agenda-files" nil "rg --follow --files-with-matches '^(CLOCK:|[*]+ +(TODO|WAIT))' roam -g '!attach' -g '!daily' > org-agenda-files")))))

(add-hook! after-save-hook
  (when (derived-mode-p 'org-mode)
    (+org-agenda-update-ids)))


;;; Reveal context around item on TAB

(add-hook! 'org-agenda-after-show-hook
  (org-overview)
  (org-reveal)
  (org-fold-show-subtree)
  (org-display-outline-path))
