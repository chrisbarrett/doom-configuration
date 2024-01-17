;;; lang/org-exts/autoload/+citations.el -*- lexical-binding: t; -*-

(defconst +citations-key-sequence-for-lit-note-capture-template "rn"
  "The key sequence for the literature note capture template.")

(defvar +citations--citekey-for-capture nil
  "Side-channel variable used to inject citation read from prompt.

It should only ever be dynamically bound.")

;;;###autoload
(defun +citations-go-to-litnote-for-citekey (key &optional attrs)
  "Function for use as `citar-open-note-function', which see.

KEY is a cite key.

ATTRS are additional attributes for the citation passed by
citar."
  (if-let* ((node (org-roam-node-from-ref (concat "@" key))))
      (org-roam-node-visit node)
    (let ((+citations--citekey-for-capture (cons key attrs)))
      (org-capture nil +citations-key-sequence-for-lit-note-capture-template))))

;;;###autoload
(defun +citation-from-capture-or-read ()
  "Get a citar reference for use in an org capture template.

If capture was triggered by trying to navigate to non-existent a
notes file, return the reference that was originally selected by
the user.

Otherwise, prompt the user for a reference."
  (or +citations--citekey-for-capture
      (citar-select-ref)))

(defun +citations-clean-bibtex-string (s)
  "Remove quoting brackets and superfluous whitespace from string S."
  (when s
    (thread-last (replace-regexp-in-string "[\"{}]+" "" s)
                 (replace-regexp-in-string "[\n\t ]+" " ")
                 (string-replace "\\" ""))))
