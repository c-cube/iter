
NAME = sequence
DOC = sequence.docdir/index.html
TARGETS = sequence.cma sequence.cmxa sequence.cmi sequence.a sexpr.cma sexpr.cmxa sexpr.cmi
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB) sequence.mli sexpr.mli

all:
	ocamlbuild tests.native $(TARGETS) $(DOC)

install: all
	ocamlfind install $(NAME) META $(INSTALL)

clean:
	ocamlbuild -clean
