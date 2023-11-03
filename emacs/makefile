DOTEMACS:=$(HOME)/.emacs.d
.PHONY: c
install:$(wildcard *.el) lisp site-lisp projects bookmarks registers ede-projects.el
	cp -rf $^ $(DOTEMACS)/
clean:;rm -rf $(DOTEMACS)/{*.el,lisp,site-lisp}
