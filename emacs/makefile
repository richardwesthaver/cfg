SRC:=~/s/src
PKGS:=$(SRC)/shed/lisp/shed.el $(SRC)/babel/babel.el
ELPA:=/srv/shed/elpa
DOTEMACS:=$(HOME)/.emacs.d

.PHONY: c
i:$(wildcard *.el);cp $^ $(DOTEMACS)
o:lisp;cp -r $< $(DOTEMACS)/;ln -sf $(ELPA) $(DOTEMACS)/lisp/elpa
c:;rm -f $(DOTEMACS)/$(wildcard *.el)

lisp:$(PKGS);cp $^ $@/
