;;; ui/leader/config.el -*- lexical-binding: t; -*-

(setq doom-localleader-key ",")

;; KLUDGE: Doom errors if this is nil, but I never want to use it.
(setq doom-leader-alt-key "M-~")

;; Make SPC u SPC u [...] possible
(map! :map universal-argument-map
      :prefix "SPC" "u" #'universal-argument-more)

(map! :leader
      :desc "M-x"                   "x"    #'execute-extended-command
      :desc "Org Capture"           "X"    #'org-capture
      :desc "Universal argument"    "u"    #'universal-argument
      :desc "Resume last search"    "r"    #'vertico-repeat

      (:when (modulep! :ui popup)
        :desc "Toggle popups"     "-"    #'+popup/toggle)

      :desc "Shell command"         "!"    #'async-shell-command
      :desc "Search for symbol in project" "*" #'+search-project-for-symbol-at-point
      :desc "Search project"               "/" #'+vertico/project-search
      :desc "rg (deadgrep)"         "S"    #'+search/deadgrep
      :desc "Kill ring"             "R"    #'consult-yank-pop
      :desc "Rotate window layout"  "|"    #'rotate-layout
      :desc "Delete window"         "q"    #'delete-window

      :desc "Prev buffer"           "TAB"  #'+swap-buffers

      :desc "Switch buffer"         "SPC"  #'consult-buffer
      :desc "Bookmark"              "@"    #'consult-bookmark
      :desc "Show bindings"         "?"    #'general-describe-keybindings

      :desc "Eval expression"       ":"    #'pp-eval-expression
      :desc "Lisp REPL"             ";"    #'ielm

      :desc "dired"                 "d"    (cmd! (dired default-directory))
      :desc "dired (other)"         "D"    #'dired-other-window


      :desc "Edit indirectly"       "'"    (general-predicate-dispatch #'poporg-dwim

                                             ;; Exit indirect edit session if active

                                             (bound-and-true-p poporg-mode) #'poporg-edit-exit
                                             (bound-and-true-p edit-indirect--overlay) #'edit-indirect-commit
                                             (bound-and-true-p org-src-mode) #'org-edit-src-exit

                                             ;; Otherwise, open indirect-edit buffer

                                             (and (derived-mode-p 'prog-mode)
                                                  ;; Are we in a string or comment? See: `parse-partial-sexp'
                                                  (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss))))
                                             #'poporg-dwim

                                             (and (derived-mode-p 'prog-mode) (region-active-p)) #'edit-indirect-region
                                             (equal (buffer-name) "*Edit Formulas*") #'org-table-fedit-finish
                                             (derived-mode-p 'org-mode) #'org-edit-special
                                             (and (derived-mode-p 'markdown-mode) (markdown-code-block-at-point-p))'markdown-edit-code-block)

      :desc "Imenu"                 "i"    #'consult-imenu

      (:prefix-map ("," . "parens")
       :desc "Go to start"          "h"    #'sp-beginning-of-sexp
       :desc "Go to end"            "l"    #'sp-end-of-sexp
       :desc "Next"                 "n"    #'sp-next-sexp
       :desc "Prev"                 "p"    #'sp-previous-sexp
       :desc "Backward up"          "<"    #'sp-backward-up-sexp
       :desc "Up"                   ">"    #'sp-up-sexp
       :desc "Convolute"            "c"    #'sp-convolute-sexp
       :desc "Kill"                 "d"    #'sp-kill-sexp
       :desc "Kill backward"        "D"    #'sp-backward-kill-sexp
       :desc "Splice (forward)"     "k"    #'sp-splice-sexp-killing-forward
       :desc "Splice (back)"        "K"    #'sp-splice-sexp-killing-backward
       :desc "Splice (around)"      "s"    #'sp-splice-sexp-killing-around
       :desc "Raise"                "r"    #'sp-raise-sexp
       :desc "Add to next"          "a"    #'sp-add-to-next-sexp
       :desc "Add to prev"          "A"    #'sp-add-to-previous-sexp
       :desc "Barf (forward)"       "b"    #'sp-forward-barf-sexp
       :desc "Barf (back)"          "B"    #'sp-backward-barf-sexp
       :desc "Slurp (forward)"      "m"    #'sp-forward-slurp-sexp
       :desc "Slurp (back)"         "M"    #'sp-backward-slurp-sexp
       :desc "Emit"                 "e"    #'sp-emit-sexp
       :desc "Join"                 "j"    #'sp-join-sexp
       :desc "Transpose"            "t"    #'sp-transpose-sexp
       :desc "Unwrap (back)"        "U"    #'sp-backward-unwrap-sexp
       :desc "Unwrap (forward)"     "u"    #'sp-unwrap-sexp
       :desc "Rewrap"               "w"    #'sp-rewrap-sexp
       :desc "Split"                "x"    #'sp-split-sexp
       :desc "Copy (forward)"       "y"    #'sp-copy-sexp
       :desc "Copy (back)"          "Y"    #'sp-backward-copy-sexp)

      (:prefix-map ("a" . "apps")
       :desc "Eshell"               "e"    #'eshell
       :desc "Quick calc"           "c"    #'quick-calc
       :desc "Calc"                 "C"    #'full-calc
       :desc "ChatGPT"              "g"    #'chatgpt-shell
       :desc "Profiler stop/start"  "r"    (general-predicate-dispatch 'profiler-start
                                             (and (featurep 'profiler) (profiler-running-p)) #'+profiler-stop-and-report)
       (:prefix-map ("d" . "debugger")
        :desc "Return to session"   "d"    #'edebug-where
        :desc "Toplevel"            "q"    #'edebug-top-level-nonstop
        :desc "Stop"                "s"    #'edebug-stop
        :desc "Help"                "?"    #'edebug-help
        :desc "Switch to eval list" "l"    #'edebug-visit-eval-list
        :desc "Unset"               "x"    #'edebug-remove-instrumentation))

      (:prefix-map ("b" . "buffers")
       :desc "Bury"                 "b"    #'bury-buffer
       :desc "Kill"                 "d"    #'kill-current-buffer
       :desc "Save"                 "w"    #'save-buffer
       :desc "Save all"             "W"    #'evil-write-all
       :desc "Switch..."            "s"    #'consult-buffer
       :desc "Switch... (other window)" "S" #'consult-buffer-other-window
       :desc "Next"                 "n"    #'next-buffer
       :desc "Previous"             "p"    #'previous-buffer
       :desc "List"                 "l"    #'ibuffer)

      (:prefix-map ("c" . "code")
       :desc "REPL"                                  ":"   #'+eval/open-repl-other-window
       :desc "REPL (same window)"                    ";"   #'+eval/open-repl-same-window
       :desc "LSP Execute code action"               "a"   #'eglot-code-actions
       :desc "Compile"                               "c"   #'compile
       :desc "Recompile"                             "C"   #'recompile
       :desc "Jump to definition"                    "d"   #'+lookup/definition
       :desc "Evaluate buffer/region"                "e"   #'+eval/buffer-or-region
       :desc "Evaluate & replace region"             "E"   #'+eval:replace-region
       :desc "Format buffer/region"                  "f"   #'+format/region-or-buffer
       :desc "Find implementations"                  "i"   #'+lookup/implementations
       :desc "Jump to symbol in current workspace"   "j"   #'consult-eglot-symbols
       :desc "Jump to documentation"                 "k"   #'+lookup/documentation
       :desc "Comment/uncomment"                     "l"   #'comment-line
       :desc "Jump to references"                    "m"   #'+lookup/references
       :desc "Send to repl"                          "s"   #'+eval/send-region-to-repl
       :desc "Find type definition"                  "t"   #'+lookup/type-definition
       :desc "Delete trailing whitespace"            "w"   #'delete-trailing-whitespace
       :desc "Delete trailing newlines"              "W"   #'doom/delete-trailing-newlines
       :desc "List errors"                           "x"   #'+default/diagnostics
       :desc "Comment (dwim)"                        "r"    (general-predicate-dispatch (cmd! (comment-dwim nil)
                                                                                              (just-one-space)
                                                                                              (evil-insert-state))
                                                              ;; comment-region does a better job
                                                              ;; of preserving paren structure.
                                                              (region-active-p) #'comment-or-uncomment-region))
      (:prefix-map ("e" . "errors & LSP")
       :desc "Next"                 "n"    #'flymake-goto-next-error
       :desc "Prev"                 "p"    #'flymake-goto-prev-error
       :desc "Goto..."              "e"    #'consult-flymake
       :desc "List (buffer)"        "l"    #'+flymake-toggle-buffer-error-list
       :desc "List (project)"       "L"    #'+flymake-toggle-project-error-list
       :desc "Rename..."            "r"    #'eglot-rename
       :desc "Format"               "f"    #'eglot-format

       (:prefix-map ("b" . "backends")
        :desc "Running"             "b"    #'flymake-running-backends
        :desc "Disabled"            "d"    #'flymake-disabled-backends
        :desc "Reporting"           "r"    #'flymake-reporting-backends))

      (:prefix-map ("f" . "files")
       :desc "Find file from here"    "/"  #'+default/find-file-under-here
       :desc "Copy dir"               "d"  #'+copy-buffer-directory
       :desc "Copy path"              "y"  #'+default/yank-buffer-path
       :desc "Copy path (from proj root)" "Y"  #'+default/yank-buffer-path-relative-to-project
       :desc "Delete buf & file"      "D"  #'+delete-current-buffer-and-file
       :desc "Find..."                "f"  #'find-file
       :desc "Find... (other window)" "F"  #'find-file-other-window
       :desc "Save"                   "s"  #'save-buffer
       :desc "Save... (interactive)"  "S"  #'save-some-buffers
       :desc "Find literally..."      "l"  #'find-file-literally
       :desc "Find as hex..."         "l"  #'hexl-find-file
       :desc "Write copy..."          "w"  #'doom/copy-this-file
       :desc "Reload"                 "v"  #'+reload-file
       :desc "Recent files..."        "r"  #'consult-recent-file
       :desc "Rename..."              "R"  #'doom/move-this-file
       :desc "Sudo find file"         "u"  #'doom/sudo-find-file
       :desc "Sudo this file"         "U"  #'doom/sudo-this-file
       (:when IS-MAC
         :desc "Open in iTerm"        "z" #'+macos/open-in-iterm-new-window))

      (:prefix-map ("g" . "git & goto")
       :desc "Nix config file"        "n"  #'+jump-to-nix-file
       :desc "Doom config file"       "e"  #'+jump-to-doomd-file
       :desc "Emacs config file"      "E"  #'+jump-to-emacsd-file
       :desc "Doom module"            "m"  #'+jump-to-doom-module
       :desc "Messages"               "?"  (cmd! (display-buffer "*Messages*"))
       :desc "Init file"              "i"  #'+jump-to-init-file
       :desc "Magit"                  "g"  #'magit-status
       :desc "Diff of file"           "d"  #'magit-diff-buffer-file
       :desc "Blame"                  "b"  #'magit-blame
       :desc "Remote: browse"         "r"  #'browse-at-remote
       :desc "Git time machine"       "t"   #'git-timemachine-toggle
       :desc "Remote: copy"           "y"  #'browse-at-remote-kill
       :desc "Open link at pt"        "o"  #'link-hint-open-link-at-point
       :desc "Log"                    "l"  #'magit-log-buffer-file
       :desc "Worktree..."            "w"  #'magit-worktree-status
       :desc "Worktree popup..."      "W"  #'magit-worktree)

      (:prefix-map ("h" . "help")
       :desc "Info"                 "i"  #'info
       :desc "Manpage"              "m"  #'+default/man-or-woman
       :desc "Face..."              "c"  #'describe-face
       :desc "Command..."           "C"  #'helpful-command
       :desc "Function..."          "f"  #'helpful-callable
       :desc "Key..."               "k"  #'helpful-key
       :desc "Mode"                 "?"  #'describe-mode
       :desc "Properties at pt"     "p"  #'describe-text-properties
       :desc "Variable..."          "v"  #'helpful-variable
       :desc "Package homepage"     "r"  #'doom/help-package-homepage
       (:prefix-map ("d" . "doom")
        :desc "Autodef"             "a"  #'doom/help-autodefs
        :desc "Module help"         "m"  #'doom/help-modules
        :desc "Doom Help"           "h"  #'doom/help
        :desc "Package..."          "p"  #'doom/help-packages
        )
       (:prefix-map ("g" . "goto")
        :desc "Face..."             "c"  #'find-face-definition
        :desc "Function..."         "f"  #'find-function
        :desc "Lisp library..."     "l"  #'find-library
        :desc "Variable..."         "v"  #'find-variable)
       (:prefix-map ("s" . "search")
        :desc "Search info"         "i"  #'consult-info
        :desc "Search docs"         "s"  #'doom/help-search
        :desc "Search load path"    "l"  #'doom/help-search-load-path
        :desc "Search news"         "n"  #'doom/help-search-news
        :desc "Search loaded files" "f"  #'doom/help-search-loaded-files))

      (:prefix-map ("k" . "kill-ring")
       :desc "Browse kill-ring"      "r"  #'consult-yank-from-kill-ring)

      (:prefix-map ("n" . "narrow")
       :desc "Edit (indirect)"  "e"  #'edit-indirect-region
       :desc "Defun"            "f"  #'narrow-to-defun
       :desc "Region"           "r"  #'narrow-to-region
       :desc "Widen "           "w"  #'widen
       :desc "Subtree"          "s"  #'org-narrow-to-subtree
       :desc "Indirect of tree" "S"  #'org-tree-to-indirect-buffer)

      (:prefix-map ("o" . "org")
       :desc "Search..."          "/"  #'org-ql-search
       :desc "Agenda"             "a"  #'+agenda-dwim
       :desc "Index file"         "i"  #'+jump-to-index-file
       :desc "Last captured"      "g"  #'org-capture-goto-last-stored
       :desc "Todo list"          "t"  #'+org-todo-list
       :desc "Tags"               "v"  #'org-tags-view
       :desc "Work"               "w"  #'timekeep-visit-node
       :desc "Capture..."         "k"  #'org-capture
       :desc "Capture... (roam)"  "K"  #'org-roam-capture
       :desc "Store link"         "l"  #'org-store-link
       :desc "Search..."          "s"  #'org-roam-search
       :desc "Roam file..."       "f"  #'+roam-node-find

       (:when (or (modulep! :lang org +roam2)
                  (modulep! :lang org-exts +roam))
         :desc "Daily Note"        "n"  #'org-roam-dailies-goto-today
         (:prefix-map ("d" . "Dailies")
          :desc "today"         "t"  #'org-roam-dailies-goto-today
          :desc "yesterday"     "y"  #'org-roam-dailies-goto-yesterday
          :desc "goto date..."  "d"  #'org-roam-dailies-goto-date))

       (:prefix-map ("c" . "clock")
        :desc "Punch in"          "i"  #'timekeep-start
        :desc "Punch out"         "o"  #'timekeep-stop
        :desc "Resolve..."        "r"  #'org-resolve-clocks
        :desc "Goto"              "g"  #'org-clock-goto)

       (:prefix-map ("r" . "roam/review")
        :desc "Recently added"    "d"  #'org-roam-review-list-recently-added
        :desc "Graph"             "g"  #'org-roam-graph
        :desc "Review notes"      "r"  #'org-roam-review
        :desc "Search (tags)..."  "t"  #'org-roam-search-tags
        :desc "Show links"        "l"  #'org-roam-links
        (:when (modulep! :lang org-exts +citations)
          :desc "Lit notes..."      "n"  #'citar-open-notes
          :desc "Open reference..." "b"  #'citar-open)))

      (:prefix-map ("p" . "project")
       :desc "Remove known project" "x" #'project-forget-project
       :desc "Edit .dir-locals" "e" (cmd!
                                     (let ((default-directory (project-root)))
                                       (find-file ".dir-locals.el")))
       :desc "Shell command..." "!"  #'project-async-shell-command
       :desc "Compile..."       "c"  #'project-compile
       :desc "Switch... (dired)" "j" (cmd! (project-switch-project "D"))
       :desc "Switch..."        "p"  (cmd!
                                      (let ((project-switch-commands (defun +switch-to-project-default ()
                                                                       (interactive)
                                                                       (let ((dir project-current-directory-override))
                                                                         (if (magit-gitdir dir)
                                                                             (magit-status dir)
                                                                           (dired dir))))))
                                        (call-interactively #'project-switch-project)))
       :desc "Find file..."     "f"  #'project-find-file
       :desc "Find dir..."      "d"  #'project-find-dir
       :desc "Switch buffer..." "b"  #'project-switch-to-buffer
       :desc "Search (rg)"      "/"  #'consult-ripgrep
       :desc "Replace"          "r"  #'project-query-replace-regexp)

      (:prefix-map ("s" . "search")
       :desc "Find defs"               "g"  #'xref-find-definitions
       :desc "Find def (other window)" "G"  #'xref-find-definitions-other-window
       :desc "Find references"         "m"  #'xref-find-references
       :desc "This buffer..."          "b"  #'+default/search-buffer
       :desc "All buffers..."          "B"  (cmd!! #'consult-line-multi 'all-buffers))

      (:prefix-map ("t" . "toggles")
       :desc "Comment visibility"      "c" #'hide/show-comments-toggle
       (:when (modulep! :ui treemacs)
         :desc "file tree"             "t" #'+treemacs/toggle)
       :desc "Fill Column Indicator"   "f" #'global-display-fill-column-indicator-mode
       (:when (modulep! :ui indent-guides)
         :desc "Indent guides"         "i" #'highlight-indent-guides-mode)
       :desc "Line numbers"            "l" #'global-display-line-numbers-mode
       :desc "Input method"            "m"  #'toggle-input-method
       :desc "Read-only mode"          "r" #'read-only-mode
       :desc "Spell checker"           "s" #'spell-fu-mode
       :desc "Soft line wrapping"      "w" #'+word-wrap-mode)

      (:prefix-map ("w" . "windows")
       :desc "Up"                      "k" #'evil-window-up
       :desc "Down"                    "j" #'evil-window-down
       :desc "Left"                    "h" #'evil-window-left
       :desc "Right"                   "l" #'evil-window-right
       ;; ---------
       (:when (modulep! :ui popup)
         :desc "Popup-to-normal" "m"   #'+popup/raise
         :desc "Next (popup)"    "z"   #'+popup/other
         :desc "Toggle popups"           "SPC" #'+popup/toggle)
       ;; ---------
       :desc "Prev"                    "p"  #'evil-window-prev
       :desc "Next"                    "w"  #'evil-window-next
       :desc "Next"                    "n"  #'evil-window-next
       :desc "Rotate"                  "r"  #'evil-window-rotate-downwards
       :desc "Jump to register..."     "s"  #'consult-register
       :desc "Save to register..."     "S"  #'window-configuration-to-register
       :desc "Split (horizontal)"      "/"  #'+split-window-horizontally-dwim
       :desc "Split (vertical)"        "-"  #'+split-window-vertically-dwim
       :desc "Balance"                 "="  #'balance-windows
       :desc "Delete"                  "d"  #'delete-window
       :desc "Delete non-dedicated"    "o"  #'+delete-nondedicated-windows
       :desc "Delete others"           "O"  #'delete-other-windows
       :desc "Toggle dedication"       "t"  #'+toggle-window-dedication)

      (:prefix-map ("y" . "snippets")
       :desc "New"                     "n"  #'yas-new-snippet
       :desc "Expand"                  "e"  #'yas-expand
       :desc "Open..."                 "f"  #'yas-visit-snippet-file
       :desc "Insert..."               "y"  #'yas-insert-snippet)

      (:prefix-map ("z" . "zoom")
       :desc "In"                      "+"  #'doom/increase-font-size
       :desc "Out"                     "-"  #'doom/decrease-font-size
       :desc "Reset"                   "="  #'doom/reset-font-size))

;; KLUDGE: Configure leader eagerly; it seems to be broken on first input
;; out-of-the-box.

(add-hook! 'after-init-hook
  (unless noninteractive
    (evil-define-key* '(normal visual motion) general-override-mode-map (kbd "SPC") 'doom/leader)
    (general-override-mode +1)
    (which-key-mode +1)))

(after! which-key
  (setq which-key-idle-delay 0.3)

  (let ((prefix-re (regexp-opt (list doom-leader-key doom-leader-alt-key))))
    (cl-pushnew `((,(format "\\`\\(?:C-w\\|%s w\\) m\\'" prefix-re))
                  nil . "maximize")
                which-key-replacement-alist)))
