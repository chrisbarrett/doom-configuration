;;; lang/org-exts/autoload/+crate-links.el -*- lexical-binding: t; -*-


(defun +crate-links--stdlib-p (path-or-href)
  (string-match-p (rx bol (or "std/"
                              (and "https://doc.rust-lang.org/"
                                   (or "stable" "nightly") "/")))
                  path-or-href))

(defun +crate-links--rustlang-url-to-title (path-or-href)
  (pcase-let* ((path (thread-last (string-remove-prefix "https://doc.rust-lang.org/" path-or-href)
                                  (string-remove-suffix "/index.html")
                                  (string-remove-suffix ".html")))
               (`(,release . ,parts) (string-split path "/")))


    (concat (if (equal release "stable")
                ""
              (format "%s/" release))
            (string-join parts "::"))))

(defun +crate-links--docs.rs-url-to-title (path-or-href)
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

;;;###autoload
(defun +crate-links-url-to-title (path-or-href)
  (if (+crate-links--stdlib-p path-or-href)
      (+crate-links--rustlang-url-to-title path-or-href)
    (+crate-links--docs.rs-url-to-title path-or-href)))


(ert-deftest test/+crate-links--parsing-crate--latest--basic ()
  (should (equal "foo::bar::fn.baz"
                 (+crate-links-url-to-title
                  "https://docs.rs/foo/latest/foo/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing-crate--version-specified ()
  (should (equal "foo@2.0.0/foo::bar::fn.baz"
                 (+crate-links-url-to-title
                  "https://docs.rs/foo/2.0.0/foo/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing-crate--crate-differs-from-ns ()
  (should (equal "fooA/fooB::bar::fn.baz"
                 (+crate-links-url-to-title
                  "https://docs.rs/fooA/latest/fooB/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing-crate--crate-differs--version-specified ()
  (should (equal "fooA@2.0.0/fooB::bar::fn.baz"
                 (+crate-links-url-to-title
                  "https://docs.rs/fooA/2.0.0/fooB/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing-crate--crate-name-convertability-with-ns ()
  (should (equal "f_o_o::bar::fn.baz"
                 (+crate-links-url-to-title
                  "https://docs.rs/f-o-o/latest/f_o_o/bar/fn.baz.html"))))

(ert-deftest test/+crate-links--parsing-crate--latest--crate-index ()
  (should (equal "foo"
                 (+crate-links-url-to-title
                  "https://docs.rs/foo/latest/foo/index.html"))))


(ert-deftest test/+crate-links--parsing-std--top-nightly ()
  (should (equal "nightly/std"
                 (+crate-links-url-to-title "https://doc.rust-lang.org/nightly/std"))))

(ert-deftest test/+crate-links--parsing-std--top-stable ()
  (should (equal "std"
                 (+crate-links-url-to-title "https://doc.rust-lang.org/stable/std"))))

(ert-deftest test/+crate-links--parsing-std--top-index ()
  (should (equal "std"
                 (+crate-links-url-to-title "https://doc.rust-lang.org/stable/std/index.html"))))




(cl-defun +crate-links--resolve (path &optional crate (version "latest"))
  (cl-labels ((to-crate (ns)
                (string-replace "_" "-" ns))
              (to-ns (crate)
                (string-replace "-" "_" crate))
              (module-doc-p (split-path)
                (string-match-p (rx bol (or "fn" "trait" "impl" "struct" "enum" "type" "macro") "." (+ nonl))
                                (-last-item split-path))))

    (let* ((split (split-string path ":" t))
           (ns (to-ns (car split)))
           (crate (to-crate (or crate ns))))
      (if (equal 1 (length split))
          (list :crate crate :version version :path path)
        (let ((parts (append split (unless (module-doc-p split)
                                     '("index")))))
          (list :crate crate
                :version version
                :path (concat (string-join parts "/") ".html")))))))

(defun +crate-links--crate-url (title)
  (-let [(&plist :path :crate :version)
         (pcase (string-split title "/")
           (`(,path)
            (+crate-links--resolve path))
           (`(,qualifier ,rest)
            (pcase (string-split qualifier "@")
              (`(,crate)
               (+crate-links--resolve rest crate))
              (`(,crate ,version)
               (+crate-links--resolve rest crate version)))))]
    (format "https://docs.rs/%s/%s/%s" crate version path)))

(defun +crate-links--rust-docs-url (title)
  (let ((version (if (string-prefix-p "nightly/" title)
                     "nightly"
                   "stable"))
        (path (->> title
                   (string-remove-prefix "nightly/")
                   (string-remove-prefix "stable/"))))
    (-let [(&plist :path :crate :version) (+crate-links--resolve path "std" version)]
      (format "https://doc.rust-lang.org/%s/%s" version path))))


;;;###autoload
(defun +crate-links-title-to-url (title)
  (if (string-match-p (rx bol (? (or "nightly" "stable") "/")
                          "std::")
                      title)
      (+crate-links--rust-docs-url title)
    (+crate-links--crate-url title)))


(ert-deftest test/+crate-links--reconstruct--latest--basic ()
  (should (equal "https://docs.rs/foo/latest/foo/bar/fn.baz.html"
                 (+crate-links-title-to-url
                  "foo::bar::fn.baz"))))

(ert-deftest test/+crate-links--reconstruct--version-specified ()
  (should (equal "https://docs.rs/fooA/2.0.0/fooB/bar/fn.baz.html"
                 (+crate-links-title-to-url
                  "fooA@2.0.0/fooB::bar::fn.baz"))))

(ert-deftest test/+crate-links--reconstruct--crate-specified ()
  (should (equal "https://docs.rs/fooA/latest/fooB/bar/fn.baz.html"
                 (+crate-links-title-to-url
                  "fooA/fooB::bar::fn.baz"))))

(ert-deftest test/+crate-links--reconstruct--crate-name-convertability ()
  (should (equal "https://docs.rs/f-o-o/latest/f_o_o/bar/fn.baz.html"
                 (+crate-links-title-to-url
                  "f_o_o::bar::fn.baz"))))

(ert-deftest test/+crate-links--round-trip--futures ()
  (let ((url "https://docs.rs/futures/latest/futures/future/index.html"))
    (should (equal url
                   (+crate-links-title-to-url
                    (+crate-links-url-to-title url))))))

(ert-deftest test/+crate-links--reconstruct--std--module-doc ()
  (should (equal
           "https://doc.rust-lang.org/stable/std/collections/index.html"
           (+crate-links-title-to-url "std::collections"))))


(ert-deftest test/+crate-links--reconstruct--std--member-doc ()
  (should (equal
           "https://doc.rust-lang.org/stable/std/vec/struct.Vec.html"
           (+crate-links-title-to-url "std::vec::struct.Vec"))))
