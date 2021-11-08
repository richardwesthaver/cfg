SRC:=$(SHED)/src
PKGS:=$(SRC)/shed/lisp/shed.el $(SRC)/babel/babel.el
CONTRIB:=$(SRC)/contrib/el
DOTEMACS:=$(HOME)/.config/emacs
DATA:=$(SHED)/data/emacs

.PHONY: c
i:early-init.el init.el;cp -r $^ $(DOTEMACS)
o:lisp;cp -r $< $(DATA)/;ln -sf $(CONTRIB) $(DATA)/lisp/contrib
c:;rm -rf $(DOTEMACS);mkdir -p $(DOTEMACS)

lisp:$(PKGS);cp $^ $@/
