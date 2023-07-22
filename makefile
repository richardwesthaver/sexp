# sxp/makefile
cl_src=sxp.asd sxp.lisp tests.lisp bench.lisp
test_input=tests.sxp
compile=--eval '(asdf:compile-system :sxp)'
load=--eval '(asdf:load-asd "sxp.asd")' --eval '(ql:quickload :sxp)'
test=--eval '(ql:quickload :sxp/tests)' --eval '(asdf:test-system :sxp)'
p1=--eval '(require :sb-sprof)' --eval '(sb-sprof:start-profiling :sample-interval 0.001)'
p2=--eval '(sb-sprof:stop-profiling)' --eval '(sb-sprof:report)'
bench=--eval '(ql:quickload :sxp/bench)' $(p1) --eval '(sxp-bench:run-bench t)' $(p2)
LISP?=sbcl --noinform --non-interactive $(load) 

all:cl rs py el
.PHONY:rs py clean

cl:compile test bench;
compile:$(cl_src); $(LISP) $(compile)
test:compile $(test_input);$(LISP) $(test)
bench:compile $(test_input);$(LISP) $(bench)
debug:compile;sbcl --noinform $(load)

rs:;cd rs && cargo build --release && cargo fmt && cargo test
rs-udp:rs;cd rs && cargo run --example udp-async

py:;cd py && poetry update && poetry build && poetry install

el:;emacs --batch --eval '(native-compile "sxp.el")' --eval '(byte-compile-file "sxp.el")'

clean:;rm -rf report.sxp *.fasl rs/target sxp.elc
