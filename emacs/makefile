DOTEMACS:=$(HOME)/.emacs.d
.PHONY: c
i:$(wildcard *.el) lisp site-lisp projects bookmarks registers;cp -rf $^ $(DOTEMACS)/
c:;rm -rf $(DOTEMACS)/{*.el,lisp,site-lisp}
all:c i
