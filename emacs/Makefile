DOTEMACS:=$(HOME)/.emacs.d
.PHONY: c
i:$(wildcard *.el) lisp;cp -r $^ $(DOTEMACS)
c:;rm -rf $(DOTEMACS)/{*.el,lisp}
