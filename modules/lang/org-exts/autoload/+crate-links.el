;;; lang/org-exts/autoload/+crate-links.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +crate-links-docs.rs-title-parse (path-or-href)
  (pcase-let* ((path (thread-last (string-remove-prefix "https://docs.rs/" path-or-href)
                                  (string-remove-suffix "/index.html")
                                  (string-remove-suffix ".html")))
               (`(,crate ,version ,ns . ,parts) (string-split path "/")))
    (if (zerop (length parts))
        crate
      (let* ((crate-and-namespace-equivalent-p (equal (string-replace "-" "_" crate) ns))
             (qualifier
              (cond
               ((and (equal version "latest") crate-and-namespace-equivalent-p)
                "")
               ((equal version "latest")
                (concat crate "/"))
               (t
                (concat crate "@" version "/")))))
        (concat qualifier ns "::" (string-join parts "::"))))))


(ert-deftest test/+crate-links--parsing--latest--basic ()
  (should (equal "foo::bar::fn.baz"
                 (+crate-links-docs.rs-title-parse
                  "https://docs.rs/foo/latest/foo/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing--version-specified ()
  (should (equal "foo@2.0.0/foo::bar::fn.baz"
                 (+crate-links-docs.rs-title-parse
                  "https://docs.rs/foo/2.0.0/foo/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing--crate-differs-from-ns ()
  (should (equal "fooA/fooB::bar::fn.baz"
                 (+crate-links-docs.rs-title-parse
                  "https://docs.rs/fooA/latest/fooB/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing--crate-differs--version-specified ()
  (should (equal "fooA@2.0.0/fooB::bar::fn.baz"
                 (+crate-links-docs.rs-title-parse
                  "https://docs.rs/fooA/2.0.0/fooB/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing--crate-name-convertability-with-ns ()
  (should (equal "f_o_o::bar::fn.baz"
                 (+crate-links-docs.rs-title-parse
                  "https://docs.rs/f-o-o/latest/f_o_o/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing--latest--crate-index ()
  (should (equal "foo"
                 (+crate-links-docs.rs-title-parse
                  "https://docs.rs/foo/latest/foo/index.html"))))



;;;###autoload
(defun +crate-links--docs.rs-title-to-url (title)
  (cl-labels ((to-crate (ns)
                (string-replace "_" "-" ns))
              (to-ns (crate)
                (string-replace "-" "_" crate))
              (reconstruct (path &optional crate (version "latest"))
                (concat "https://docs.rs/"
                        (let* ((split (split-string path ":" t))
                               (ns (to-ns (car split)))
                               (crate (to-crate (or crate ns))))
                          (if (equal 1 (length split))
                              (string-join (list crate version ns "index.html")
                                           "/")
                            (let* ((last-item (car (last split)))
                                   (module-doc-p (string-match-p (rx bol (or "fn" "trait" "impl" "struct" "enum" "type" "macro") "." (+ nonl))
                                                                 last-item))
                                   (parts (append (list crate version)
                                                  split
                                                  (unless module-doc-p
                                                    '("index")))))
                              (concat (string-join parts "/") ".html")))))))

    (pcase (string-split title "/")
      (`(,path)
       (reconstruct path))
      (`(,qualifier ,rest)
       (pcase (string-split qualifier "@")
         (`(,crate)
          (reconstruct rest crate))
         (`(,crate ,version)
          (reconstruct rest crate version)))))))


(ert-deftest test/+crate-links--reconstruct--latest--basic ()
  (should (equal "https://docs.rs/foo/latest/foo/bar/fn.baz.html"
                 (+crate-links--docs.rs-title-to-url
                  "foo::bar::fn.baz"))))

(ert-deftest test/+crate-links--reconstruct--version-specified ()
  (should (equal "https://docs.rs/fooA/2.0.0/fooB/bar/fn.baz.html"
                 (+crate-links--docs.rs-title-to-url
                  "fooA@2.0.0/fooB::bar::fn.baz"))))

(ert-deftest test/+crate-links--reconstruct--crate-specified ()
  (should (equal "https://docs.rs/fooA/latest/fooB/bar/fn.baz.html"
                 (+crate-links--docs.rs-title-to-url
                  "fooA/fooB::bar::fn.baz"))))

(ert-deftest test/+crate-links--reconstruct--crate-name-convertability ()
  (should (equal "https://docs.rs/f-o-o/latest/f_o_o/bar/fn.baz.html"
                 (+crate-links--docs.rs-title-to-url
                  "f_o_o::bar::fn.baz"))))

(ert-deftest test/+crate-links--round-trip--futures ()
  (let ((url "https://docs.rs/futures/latest/futures/future/index.html"))
    (should (equal url
                   (+crate-links--docs.rs-title-to-url
                    (+crate-links-docs.rs-title-parse url))))))
