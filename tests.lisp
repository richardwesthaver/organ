(defpackage :organ-tests
  (:use :cl :organ :organ-cli :fiveam)
  (:shadowing-import-from #:fiveam #:test))

(in-package :organ-tests)

(def-suite organ
  :description "Organ test-suite.")
(in-suite organ)
(test hello-world "hello world" (is (string= "test" "test")))
(def-suite organ-cli :in organ)
