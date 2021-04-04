# OCAMLC = ocamlopt -noassert -unsafe
OCAMLC = ocamlopt -g

ALL_SRC=$(wildcard *.ml)
EXEC=$(shell ocamldep -sort $(EXEC_SRC))
EXEC_MOD=$(EXEC:.ml=.cmx)

TEX=$(wildcard *.tex)
PDF=$(TEX:.tex=.pdf)

default: $(EXEC_MOD) pingouin $(PDF)

pingouin: $(EXEC_MOD)
	echo $(EXEC_SRC)
	if [ -f main.ml ] ; then \
	  $(OCAMLC) $(EXEC_MOD) -o pingouin ; \
	fi

SOURCES = $(wildcard *.ml) $(wildcard *mli)
.depend: $(SOURCES)
	ocamldep $(SOURCES) > .depend
-include .depend

%.cmx: %.ml Makefile
	$(OCAMLC) -c $<
%.cmi: %.mli Makefile
	$(OCAMLC) -c $<

tarball:
	rm -rf prog2_pingouin
	mkdir prog2_pingouin
	cp *.mli Makefile prog2_pingouin
	mkdir prog2_pingouin/problems
	cp problems/* prog2_pingouin/problems
	tar zcvf prog2_pingouin.tar.gz prog2_pingouin/

clean:
	rm -f pingouin
	rm -f *.cmx *.cmo *.cmi *.o
.PHONY: test clean

%.pdf: %.tex
	rubber -d $<
