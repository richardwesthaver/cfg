DOTSTUMPWM:=$(HOME)/.stumpwm.d
.PHONY: c
i:$(wildcard *.lisp);cp -rf $^ $(DOTSTUMPWM)/
c:;rm -rf $(DOTSTUMPWM)/*.lisp
all:c i
