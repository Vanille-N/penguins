OCAMLFAST = ocamlopt -noassert -unsafe
OCAMLC = ocamlopt -g

ALL_SRC=$(wildcard *.ml)
TEST_SRC=$(wildcard *test*)
PERF_SRC=$(wildcard *perf*)
EXEC_SRC=$(filter-out $(TEST_SRC) $(PERF_SRC), $(ALL_SRC))
EXEC=$(shell ocamldep -sort $(EXEC_SRC))
TEST=$(shell ocamldep -sort $(filter-out main.ml $(PERF_SRC), $(ALL_SRC)))
PERF=$(shell ocamldep -sort $(filter-out main.ml $(TEST_SRC), $(ALL_SRC)))
EXEC_MOD=$(EXEC:.ml=.cmx)
TEST_MOD=$(TEST:.ml=.cmx)
PERF_MOD=$(PERF:.ml=.cmx)

TEX=$(wildcard *.tex)
PDF=$(TEX:.tex=.pdf)

default: $(EXEC_MOD) pingouin $(PDF)

pingouin: $(EXEC_MOD)
	$(OCAMLC) $(EXEC_MOD) -o pingouin

test: $(TEST_MOD)
	$(OCAMLC) $(TEST_MOD) -o test
	./test

perf: $(PERF_MOD)
	$(OCAMLFAST) $(PERF_MOD) -o perf
	./perf

prof: $(PERF_MOD)
	ocamloptp -P $(PERF_MOD) -o prof
	perf record --call-graph=dwarf -- ./prof
	perf report	
	ocamlcp -p a $(PERF) -o prof
	./prof
	ocamlprof priority.ml

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

.PHONY: test clean perf prof

%.pdf: %.tex
	rubber -d $<
