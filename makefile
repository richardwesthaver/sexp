# sxp/makefile
src=sxp.lisp tests.lisp bench.lisp
compile=--eval '(asdf:compile-system :sxp)'
load=--eval '(asdf:load-asd "sxp.asd")' --eval '(ql:quickload :sxp)'
test=--eval '(ql:quickload :sxp/tests)' --eval '(asdf:test-system :sxp)'
bench=--eval '(ql:quickload :sxp/bench)' --eval '(sxp-bench:run-bench t)'
LISP?=sbcl --noinform --non-interactive $(load) 
all:compile test bench;
compile:$(src); $(LISP) $(compile)
test:compile;$(LISP) $(test)
bench:compile;$(LISP) $(bench)
debug:compile;sbcl --noinform $(load)
clean:;rm -rf report.sxp
