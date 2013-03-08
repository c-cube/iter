
NAME = sequence
DOC = sequence.docdir/index.html
TARGETS = sequence.cma sequence.cmxa sequence.cmi sequence.a
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB) sequence.mli

all:
	ocamlbuild $(TARGETS) $(DOC)

benchs: all
	ocamlbuild -use-ocamlfind -pkg bench -pkg unix tests/benchs.native

tests:
	ocamlbuild -use-ocamlfind -pkg oUnit tests/run_tests.native

install: all
	ocamlfind install $(NAME) META $(INSTALL)

clean:
	ocamlbuild -clean

.PHONY: all clean tests benchs
