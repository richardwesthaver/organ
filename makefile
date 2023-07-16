# organ/makefile
LISP?=sbcl --noinform --non-interactive
src?=organ.lisp cli.lisp tests/tests.lisp
load?=--eval '(asdf:load-asd "organ.asd")'
build?=--eval '(asdf:make "organ/cli")'
test?=--eval '(asdf:test-system "organ")'
all:test build;
test:$(src);$(LISP) $(load) $(test)
build:$(src);$(LISP) $(build)
