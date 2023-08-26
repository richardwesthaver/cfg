DOTNYXT:=$(HOME)/.config/nyxt
.PHONY: c
i:$(wildcard *.lisp);cp -rf $^ $(DOTNYXT)/
c:;rm -rf $(DOTNYXT)/*.lisp
all:c i
