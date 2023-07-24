(defsystem organ
  :depends-on ("sxp" "cl-ppcre")
  :version "0.1.0"
  :description "org-mode utils"
  :in-order-to ((test-op (test-op :organ/tests)))
  :components ((:file "organ")))

(defsystem organ/cli
  :depends-on ("organ" "clingon")
  :version "0.1.0"
  :build-operation "program-op"
  :build-pathname "bin/organ"
  :entry-point "organ-cli:main"
  :components ((:file "cli")))

(defsystem organ/tests
  :depends-on ("organ" "organ/cli" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :organ-tests '#:test-all)))
