
NAME = sequence
DOC = sequence.docdir/index.html
TARGETS = sequence.cma sequence.cmxa sequence.cmi sequence.a
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB) sequence.mli

all: tests
	ocamlbuild $(TARGETS) $(DOC)

bench: all
	ocamlbuild -use-ocamlfind -pkg bench tests/benchs.native

tests:
	ocamlbuild -use-ocamlfind -pkg oUnit tests/run_tests.native

install: all
	ocamlfind install $(NAME) META $(INSTALL)

clean:
	ocamlbuild -clean

.PHONY: all clean tests bench
