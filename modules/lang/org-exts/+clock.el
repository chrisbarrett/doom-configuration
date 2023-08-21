;;; lang/org-exts/+clock.el -*- lexical-binding: t; -*-

(setq org-clock-history-length 20)
(setq org-clock-in-resume t)
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil)
(setq org-clock-report-include-clocking-task t)
(setq org-clock-mode-line-total 'today)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-redeadline 'time)
(setq org-log-repeat 'time)
(setq org-log-reschedule 'time)
(setq org-reverse-note-order nil)

(org-clock-persistence-insinuate)
