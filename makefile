# makefile --- cfg Makefile
L?=sbcl --noinform --non-interactive --eval '(asdf:load-asd "cfg.asd")' --eval '(ql:quickload :cfg)'
src=cfg.lisp
compile=--eval '(asdf:compile-system :cfg)'
all:clean compile
.PHONY:clean
compile:$(src);$(L) $(compile)
clean:;rm -rf *.fasl
