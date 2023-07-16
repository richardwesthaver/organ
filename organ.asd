(defsystem organ
  :name "organ"
  :description "org-mode utils"
  :components ((:file "organ")))

(defsystem organ/cli
  :depends-on ("organ" "clingon")
  :build-operation "program-op"
  :build-pathname "organ"
  :entry-point "organ-cli:main"
  :components ((:file "cli")))
