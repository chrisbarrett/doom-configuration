;;; compat/nix/config.el -*- lexical-binding: t; -*-

;;; Set PATH correctly on Darwin

;; macOS prevents PATH being modified for graphical apps, so the wrapper set up
;; with Nix won't work. Use another environment variable to pass through the
;; desired value.

(when-let* ((path-extras (and IS-MAC (getenv "NIX_EMACS_DARWIN_PATH_EXTRAS"))))

  (dolist (dir (parse-colon-path path-extras))
    (push dir exec-path))
  (setq exec-path (seq-uniq exec-path))

  (autoload 'string-join "subr-x")
  (setenv "PATH" (string-join exec-path ":")))

;; Use plantuml supplied by Nix

(when-let* ((jar (getenv "NIX_EMACS_PLANTUML_JAR")))
  (after! plantuml
    ;; Gotta do it twice to shut up doom doctor.
    (setq plantuml-jar-path (concat doom-data-dir "plantuml.jar"))
    (setq org-plantuml-jar-path plantuml-jar-path)))

(when-let* ((program (getenv "NIX_EMACS_TEX_PROGRAM")))
  (after! tex
    (setq TeX-command program)))
