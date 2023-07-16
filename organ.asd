(defsystem organ
  :depends-on ("sxp")
  :description "org-mode utils"
  :in-order-to ((test-op (test-op :organ/tests)))
  :components ((:file "organ")))

(defsystem organ/cli
  :depends-on ("organ" "clingon")
  :build-operation "program-op"
  :build-pathname "organ"
  :entry-point "organ-cli:main"
  :components ((:file "cli")))

(defsystem organ/tests
  :depends-on ("organ" "fiveam")
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :fiveam :run! :organ)))
