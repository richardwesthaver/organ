(defpackage :organ-tests
  (:use :cl :organ :organ-cli :fiveam)
  (:import-from :clingon :parse-command-line :getopt)
  (:shadowing-import-from #:fiveam #:test))

(in-package :organ-tests)

(def-suite organ
  :description "Organ test-suite.")
(in-suite organ)
(test org->sxp
  (is nil))
(test sxp->org
  (is nil))

(def-suite* organ-cli :in organ)

(test sanity
  (let ((cmd (parse-command-line *organ-cmd* '("-i" "foo" "-o" "bar"))))
    (is (string= (getopt cmd :input) "foo"))
    (is (string= (getopt cmd :output) "bar"))))

(test input (is (parse-command-line *organ-cmd* '("test.org"))))
(test output (is (parse-command-line *organ-cmd* '("test.org" "file.sxp"))))
