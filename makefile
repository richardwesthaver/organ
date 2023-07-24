# organ/makefile
LISP?=sbcl --noinform --non-interactive $(load) 
src=organ.lisp cli.lisp tests.lisp
compile=--eval '(asdf:compile-system :organ)'
load=--eval '(asdf:load-asd "organ.asd")' --eval '(ql:quickload :organ)'
build=--eval '(ql:quickload :organ/cli)' --eval '(asdf:make :organ/cli)'
test=--eval '(ql:quickload :organ/tests)' --eval '(asdf:test-system :organ)'
all:compile build test;
compile:$(src); $(LISP) $(compile)
build:compile;mkdir -p bin;$(LISP) $(build)
test:compile;$(LISP) $(test)
debug:compile;sbcl --noinform $(load)
clean:;rm -rf bin *.fasl
