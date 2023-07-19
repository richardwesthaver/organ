(defpackage :organ-tests
  (:use :cl :organ :organ-cli :fiveam)
  (:import-from :clingon :parse-command-line :getopt)
  (:shadowing-import-from #:fiveam #:test)
  (:export :test-all))

(in-package :organ-tests)

(def-suite :organ
  :description "Organ test-suite.")
(in-suite :organ)
(test org-file (is (read-org-file "readme.org")))
(test org->sxp
  (is nil))
(test sxp->org
  (is nil))

(def-suite* :organ-cli :in :organ)

(test sanity
  (let ((cmd (parse-command-line (cmd) nil)))
    (is (string= "organ" (clingon:command-name cmd)))))

(test subcmds
  (is (eql
       (clingon:command-handler
	(parse-command-line (cmd) '("--input" "test.org")))
       #'organ-cli:handler))
  (is (eql
       (clingon:command-handler (parse-command-line (cmd) '("props" "test.org")))
       #'organ-cli:props-handler)))

(test output (is (parse-command-line (cmd) '("test.org" "file.sxp"))))

(defun test-all () (fiveam:run! :organ))
