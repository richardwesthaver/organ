# organ/makefile
LISP?=sbcl
.PHONY: build
build:;sbcl --noinform --non-interactive \
	--eval '(asdf:load-asd "organ.asd")' \
	--eval '(asdf:make "organ/cli")'
