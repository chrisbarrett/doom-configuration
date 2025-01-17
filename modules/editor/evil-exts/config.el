;;; editor/evil-exts/config.el -*- lexical-binding: t; -*-

;; Doom binds `s' to evil-snipe, which is not something I want to use.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(put 'evil-shift-width 'safe-local-variable 'integerp)

(map! :map (evil-ex-completion-map evil-ex-search-keymap)
      :after evil
      "C-a" #'evil-beginning-of-line
      "C-b" #'evil-backward-char
      )

(define-key! :keymaps +default-minibuffer-maps
  [escape] #'abort-recursive-edit
  "C-a"    #'move-beginning-of-line
  "C-r"    #'evil-paste-from-register
  "C-u"    #'evil-delete-back-to-indentation
  "C-v"    #'yank
  "C-w"    #'doom/delete-backward-word
  )

(after! evil
  (setq evil-v$-excludes-newline t)

  (map!
   (:prefix "g"
    :n "+" #'evil-numbers/inc-at-pt
    :n "-" #'evil-numbers/dec-at-pt
    :n "C-a" #'evil-numbers/inc-at-pt-incremental
    :n "C-x" #'evil-numbers/dec-at-pt-incremental)

   :v "v" (general-predicate-dispatch #'evil-multiedit-match-all
            (equal last-command 'evil-visual-char) (cmd!
                                                    (evil-normal-state)
                                                    (unless (eolp)
                                                      (forward-char -1))
                                                    (evil-multiedit-match-all)))

   (:when (modulep! :editor rotate-text)
     :n "]r"  #'rotate-text
     :n "[r"  #'rotate-text-backward)

   (:when (modulep! :editor multiple-cursors)
     :v  "R"     #'evil-multiedit-match-all
     :n  "M-d"   #'evil-multiedit-match-symbol-and-next
     :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
     :v  "M-d"   #'evil-multiedit-match-and-next
     :v  "M-D"   #'evil-multiedit-match-and-prev
     :nv "C-M-d" #'evil-multiedit-restore
     (:after evil-multiedit
             (:map evil-multiedit-mode-map
              :nv "M-d" #'evil-multiedit-match-and-next
              :nv "M-D" #'evil-multiedit-match-and-prev
              [return]  #'evil-multiedit-toggle-or-restrict-region))
     (:prefix ("gz" . "multi-cursor")
      :nv "d" #'evil-mc-make-and-goto-next-match
      :nv "D" #'evil-mc-make-and-goto-prev-match
      :nv "s" #'evil-mc-skip-and-goto-next-match
      :nv "S" #'evil-mc-skip-and-goto-prev-match
      :nv "c" #'evil-mc-skip-and-goto-next-cursor
      :nv "C" #'evil-mc-skip-and-goto-prev-cursor
      :nv "j" #'evil-mc-make-cursor-move-next-line
      :nv "k" #'evil-mc-make-cursor-move-prev-line
      :nv "m" #'evil-mc-make-all-cursors
      :nv "n" #'evil-mc-make-and-goto-next-cursor
      :nv "N" #'evil-mc-make-and-goto-last-cursor
      :nv "p" #'evil-mc-make-and-goto-prev-cursor
      :nv "P" #'evil-mc-make-and-goto-first-cursor
      :nv "q" #'evil-mc-undo-all-cursors
      :nv "t" #'+multiple-cursors/evil-mc-toggle-cursors
      :nv "u" #'+multiple-cursors/evil-mc-undo-cursor
      :nv "z" #'+multiple-cursors/evil-mc-toggle-cursor-here
      :v  "I" #'evil-mc-make-cursor-in-visual-selection-beg
      :v  "A" #'evil-mc-make-cursor-in-visual-selection-end)))

  (map! :map prog-mode-map
        "M-q" (general-predicate-dispatch #'+format/region-or-buffer
                (ppss-comment-or-string-start (syntax-ppss)) #'fill-paragraph))

  (map! :map evil-multiedit-mode-map
        :n "Y" (cmd!
                (when-let* ((str (iedit-current-occurrence-string)))
                  (kill-new str)
                  (message "Copied to kill ring")))
        :n "<tab>" #'iedit-toggle-selection
        :n "n" #'evil-multiedit-next
        :n "N" #'evil-multiedit-prev
        :n "S" #'evil-multiedit--change-line
        :n "<escape>" #'evil-multiedit-abort)


  (map! :map evil-window-map
        "/"   (cmd! (split-window-horizontally)
                    (let ((target-window (next-window)))
                      (set-window-buffer target-window (other-buffer))
                      (select-window target-window)))
        "-"   (cmd! (split-window-vertically)
                    (let ((target-window (next-window)))
                      (set-window-buffer target-window (other-buffer))
                      (select-window target-window)))
        "o"   #'delete-other-windows
        "SPC" #'doom/window-enlargen)

  (map! :map prog-mode-map
        :vn "(" (cmd! (beginning-of-defun) (back-to-indentation))
        :vn ")" #'end-of-defun)

  (map! :map evil-surround-mode-map
        :v "s" #'evil-surround-region)
  )

(after! evil-surround
  (add-hook! 'magit-mode-hook (evil-surround-mode -1))

  (setf (alist-get ?` evil-surround-pairs-alist)
        (fn! (cons "`" (if (derived-mode-p 'emacs-lisp-mode)
                           "'"
                         "`"))))

  (setf (alist-get ?\( evil-surround-pairs-alist) (cons "(" ")"))
  )

;; KLUDGE: Doom doesn't correctly load evil surround; do it ourselves when we
;; enter visual state.
(define-advice evil-visual-state (:before (&rest _) load-evil-surround)
  (require 'evil-surround))

;;; Teach evil-ret to open links at point

(add-hook 'prog-mode-hook #'goto-address)

(map! :after evil :n "RET" #'+evil-ret)

;; HACK: Work around change to calling convention that breaks < & > in
;; evil-org.

(after! (:all org evil-org)
  (define-advice org-get-limited-outline-regexp (:filter-args (&rest args) fix-evil-org)
    nil))

;; KLUDGE: Enable undo-tree manually; no idea why this isn't being applied
;; correctly by doom.
(autoload 'turn-on-undo-tree-mode "undo-tree")
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
