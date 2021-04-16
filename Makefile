OCAMLC = ocamlopt -g -I src -unsafe

ALL_SRC=$(wildcard src/*.ml)
TEST_SRC=$(wildcard src/*test*)
PERF_SRC=$(wildcard src/*perf*)
EXEC_SRC=$(filter-out $(TEST_SRC) $(PERF_SRC), $(ALL_SRC))
EXEC=$(shell ocamldep -sort $(EXEC_SRC))
TEST=$(shell ocamldep -sort $(filter-out src/main.ml $(PERF_SRC), $(ALL_SRC)))
PERF=$(shell ocamldep -sort $(filter-out src/main.ml $(TEST_SRC), $(ALL_SRC)))
EXEC_MOD=$(EXEC:.ml=.cmx)
TEST_MOD=$(TEST:.ml=.cmx)
PERF_MOD=$(PERF:.ml=.cmx)

TEX=$(wildcard *.tex)
PDF=$(TEX:.tex=.pdf)

default: $(EXEC_MOD) pingouin

pingouin: $(EXEC_MOD)
	$(OCAMLC) $(EXEC_MOD) -o pingouin

test: $(TEST_MOD)
	$(OCAMLC) $(TEST_MOD) -o test
	./test

perf: $(PERF_MOD)
	$(OCAMLC) $(PERF_MOD) -o perf
	./perf

report:
	cd tex ; \
	pdflatex --interaction=nonstopmode --halt-on-error README.tex ; \
	mv README.pdf ..

doc: $(PERF_MOD) $(TEST_MOD) $(EXEC_MOD)
	# module dependencies
	cd src ; \
	ocamldoc -dot -dot-include-all $(notdir $(EXEC))
	dot src/ocamldoc.out -Tpdf > tex/exec-deps.pdf
	# documentation
	mkdir -p doc
	cd src ; \
	ocamldoc -html -d ../doc *.ml *.mli


SOURCES = $(wildcard src/*.ml) $(wildcard src/*.mli)
.depend: Makefile $(SOURCES)
	ocamldep -native -I src $(SOURCES) > .depend
-include .depend

%.cmx: %.ml Makefile
	$(OCAMLC) -c $<
%.cmi: %.mli Makefile
	$(OCAMLC) -c $<

clean:
	rm -f pingouin perf test
	rm -f src/*.cmx src/*.cmo src/*.cmi src/*.o src/*.out
	rm -f perf.data* *.dump
	rm tex/*.aux tex/*.log
	rm src/ocamldoc.out

.PHONY: test clean perf

