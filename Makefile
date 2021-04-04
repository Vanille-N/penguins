# OCAMLC = ocamlopt -noassert -unsafe
OCAMLC = ocamlopt -g

ALL_SRC=$(wildcard *.ml)
TEST_SRC=$(wildcard test*)
EXEC_SRC=$(filter-out $(TEST_SRC), $(ALL_SRC))
EXEC=$(shell ocamldep -sort $(EXEC_SRC))
TEST=$(filter-out main.ml, $(ALL_SRC))
EXEC_MOD=$(EXEC:.ml=.cmx)
TEST_MOD=$(TEST:.ml=.cmx)

TEX=$(wildcard *.tex)
PDF=$(TEX:.tex=.pdf)

default: $(EXEC_MOD) pingouin $(PDF)

pingouin: $(EXEC_MOD)
	echo $(EXEC_SRC)
	if [ -f main.ml ] ; then \
	  $(OCAMLC) $(EXEC_MOD) -o pingouin ; \
	fi

test: $(TEST_MOD)
	$(OCAMLC) $(TEST_MOD) -o test
	./test

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
