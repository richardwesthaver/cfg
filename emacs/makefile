SRC:=$(SHED)/src
CFG:=$(SRC)/cfg/emacs
CONTRIB:=$(SRC)/contrib/el
DOTEMACS:=$(HOME)/.config/emacs
DATA:=$(SHED)/data/emacs

.PHONY: c

o:lisp shed.el babel.el;cp -r $< $(DATA)/;ln -sf $(CONTRIB) $(DATA)/lisp/contrib
shed.el:$(SRC)/shed/lisp/shed.el lisp;cp $< lisp/
babel.el:$(SRC)/babel/babel.el lisp; cp $< lisp/
c:;rm -rf $(DOTEMACS); ln -s $(CFG) $(DOTEMACS)
