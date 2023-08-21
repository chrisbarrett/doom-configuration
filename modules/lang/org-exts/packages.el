;;; lang/org-exts/packages.el -*- no-byte-compile: t; -*-

(package! org-appear)
(package! org-cliplink)
(package! org-ql)
(package! orgtbl-aggregate)
(package! page-break-lines)
(package! poporg)

(when (modulep! +citations)
  (package! citar)
  (package! all-the-icons)
  (when (modulep! +roam)
    (package! citar-org-roam)))

(when (modulep! +nursery)
  (package! nursery
    :recipe (:host github :repo "chrisbarrett/nursery"
             :files ("lisp/*.el"))))

(package! ox-gfm)

(when (modulep! +modern)
  (package! org-modern))
