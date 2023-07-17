# organ/makefile
src?=organ.lisp cli.lisp tests.lisp
load?=--eval '(asdf:load-asd "organ.asd")' --eval '(ql:quickload :organ)' --eval '(ql:quickload :organ/cli)' --eval '(ql:quickload :organ/tests)'
test?=--eval '(in-package :organ-tests)' --eval "(run! 'organ)"
build?=--eval '(asdf:make "organ/cli")'
LISP?=sbcl --noinform --non-interactive $(load) 
all:build test;
build:;mkdir bin;$(LISP) $(build)
test:;$(LISP) $(test)
debug:;sbcl --noinform $(load)
clean:;rm -rf bin
