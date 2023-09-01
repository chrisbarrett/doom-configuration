;;; tools/nix/autoload.el -*- lexical-binding: t; -*-

(require 'url-parse)

(defun +nix--guess-prefetch-command-for-url (str)
  (let* ((url (url-generic-parse-url str))
         (path (split-string (url-filename url) "/" t)))
    (or
     (when (equal (url-host url) "github.com")
       (pcase path
         (`(,owner ,repo ,(or "commit" "tree") ,rev)
          (list "nix-prefetch" "fetchFromGitHub" "--owner" owner "--repo" repo "--rev" rev))
         (`(,owner ,repo)
          (list "nix-prefetch""fetchFromGitHub" "--owner" owner "--repo" repo "--rev" "HEAD"))))
     (list "nix-prefetch" str))))

;;;###autoload
(defun +nix-copy-sha (input)
  "Read a URL as INPUT and prefetch its SHA for Nix."
  (interactive (list (+read-url)))
  (let* ((command (+nix--guess-prefetch-command-for-url input))
         (buf (with-current-buffer (get-buffer-create "*nix-prefetch*")
                (erase-buffer)
                (current-buffer)))
         (proc (apply #'start-process "nix-prefetch" buf command)))
    (message "Prefetching with command: %s" (string-join (seq-map #'shell-quote-argument command) " "))
    (set-process-sentinel proc
                          (lambda (p state)
                            (with-current-buffer buf
                              (cond ((and (zerop (process-exit-status p))
                                          (string-match-p "finished" state))
                                     (goto-char (point-max))
                                     (unless (search-backward-regexp (rx bol "sha"))
                                       (error "SHA not found in buffer"))
                                     (let ((sha (buffer-substring (line-beginning-position) (line-end-position))))
                                       (kill-new sha)
                                       (message "SHA copied to kill ring: %s" sha))
                                     (kill-buffer buf))
                                    (t
                                     (display-buffer buf)
                                     (error "Downloading SHA for %s failed" input))))))))
