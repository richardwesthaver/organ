(defpackage :organ-tests
  (:use :cl :organ :organ-cli :fiveam)
  (:import-from :clingon :parse-command-line)
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

(test sanity (is (parse-command-line (start) '("-h"))))
(test input (is (parse-command-line (start) '("test.org"))))
(test output (is (parse-command-line (start) '("test.org" "file.sxp"))))
