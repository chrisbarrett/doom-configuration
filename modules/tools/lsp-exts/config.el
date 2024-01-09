;; -*- lexical-binding: t; -*-

(after! eglot
  (setq eglot-confirm-server-initiated-edits nil)
  (setq eglot-extend-to-xref t))

(map! :map eglot-mode-map
      :after eglot
      "C-c C-r" 'eglot-rename
      "M-RET" 'eglot-code-actions)

(add-hook! eglot-managed-mode
  (eglot-inlay-hints-mode -1))

;; TODO: Remove once bug is properly fixed.
;; See:
;;   https://github.com/seagle0128/doom-modeline/issues/687

(define-advice doom-modeline-update-eglot (:override () 29-1-compat)
  "Update `eglot' state."
  (setq doom-modeline--eglot
        (pcase-let* ((server (and (eglot-managed-p) (eglot-current-server)))
                     (nick (and server (eglot--project-nickname server)))
                     (pending (and server
                                   (when-let* ((continuations (jsonrpc--continuations server)))
                                     (hash-table-count continuations))))
                     (last-error (and server (jsonrpc-last-error server)))
                     (face (cond (last-error 'doom-modeline-lsp-error)
                                 ((and pending (cl-plusp pending)) 'doom-modeline-lsp-warning)
                                 (nick 'doom-modeline-lsp-success)
                                 (t 'doom-modeline-lsp-warning)))
                     (icon (doom-modeline-lsp-icon "EGLOT" face)))
          (propertize icon
                      'help-echo (cond
                                  (last-error
                                   (format "EGLOT\nAn error occured: %s
mouse-3: Clear this status" (plist-get last-error :message)))
                                  ((and pending (cl-plusp pending))
                                   (format "EGLOT\n%d outstanding requests" pending))
                                  (nick (format "EGLOT Connected (%s/%s)
C-mouse-1: Go to server errors
mouse-1: Go to server events
mouse-2: Quit server
mouse-3: Reconnect to server" nick (eglot--major-modes server)))
                                  (t "EGLOT Disconnected
mouse-1: Start server"))
                      'mouse-face 'doom-modeline-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (cond (last-error
                                          (define-key map [mode-line mouse-3]
                                                      #'eglot-clear-status))
                                         ((and pending (cl-plusp pending))
                                          (define-key map [mode-line mouse-3]
                                                      #'eglot-forget-pending-continuations))
                                         (nick
                                          (define-key map [mode-line C-mouse-1]
                                                      #'eglot-stderr-buffer)
                                          (define-key map [mode-line mouse-1]
                                                      #'eglot-events-buffer)
                                          (define-key map [mode-line mouse-2]
                                                      #'eglot-shutdown)
                                          (define-key map [mode-line mouse-3]
                                                      #'eglot-reconnect))
                                         (t (define-key map [mode-line mouse-1]
                                                        #'eglot)))
                                   map)))))
