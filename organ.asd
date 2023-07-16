(defsystem organ
  :name "organ"
  :depends-on ("sxp")
  :description "org-mode utils"
  :components ((:file "organ")))

(defsystem organ/cli
  :name "organ/cli"
  :depends-on ("organ" "clingon")
  :build-operation "program-op"
  :build-pathname "organ"
  :entry-point "organ-cli:main"
  :components ((:file "cli")))

(defsystem organ/tests
  :name "organ/tests"
  :depends-on ("organ" "organ-cli" "fiveam")
  :components ((:module "tests"
		:serial t
		:components ((:file "tests"))))
  :perform (test-op (op c) (uiop:symbol-call :organ-tests :run-tests)))
