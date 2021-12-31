SRC:=~/s/src
PKGS:=$(SRC)/shed/lisp/shed.el $(SRC)/babel/babel.el
ELPA:=/srv/shed/elpa
DOTEMACS:=$(HOME)/.emacs.d

.PHONY: c
i:early-init.el init.el;cp -r $^ $(DOTEMACS)
o:lisp;cp -r $< $(DOTEMACS)/;ln -sf $(ELPA) $(DOTEMACS)/lisp/elpa
c:;rm -r $(DOTEMACS)/{lisp,early-init.el,init.el}

lisp:$(PKGS);cp $^ $@/
