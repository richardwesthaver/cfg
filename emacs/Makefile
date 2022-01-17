DOTEMACS:=$(HOME)/.emacs.d
.PHONY: c
i:$(wildcard *.el);cp $^ $(DOTEMACS)
o:lisp;cp -r $< $(DOTEMACS)/
c:;rm -f $(DOTEMACS)/{$(wildcard *.el),lisp}
