SRC:=$(SHED)/src
CONTRIB:=$(SRC)/contrib/el
DOTEMACS:=$(HOME)/.config/emacs
DATA:=$(SHED)/data/emacs

.PHONY: c
i:early-init.el init.el;cp $^ $(DOTEMACS)
o:lisp;cp -r $< $(DATA)/;ln -sf $(CONTRIB) $(DATA)/lisp/contrib
c:;rm -rf $(DOTEMACS);mkdir -p $(DOTEMACS)

lisp:shed.el babel.el
shed.el:$(SRC)/shed/lisp/shed.el lisp;cp $< lisp/$@
babel.el:$(SRC)/babel/babel.el lisp; cp $< lisp/$@

