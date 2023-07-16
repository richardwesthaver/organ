(defpackage :organ-tests
  (:use :organ :organ-cli :fiveam)
  (:export :run-tests))

(in-package :organ-tests)

(def-suite organ
  :description "Organ test-suite.")

(def-suite* organ-cli :in organ)

(defun run-tests ()
  (princ "hello world!")
  (run! :organ))
