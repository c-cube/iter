
NAME = sequence
DOC = sequence.docdir/index.html
TARGETS = sequence.cma sequence.cmxa sequence.cmi sequence.a
LIB = $(addprefix _build/, $(TARGETS)) 
INSTALL = $(LIB) sequence.mli

all:
	ocamlbuild $(TARGETS) $(DOC)

bench: all
	ocamlbuild \
		-cflags -I,`ocamlfind query extlib` \
		-lflags -I,`ocamlfind query extlib` \
		-package extlib -package unix bench.native

tests:
	ocamlbuild tests.native

install: all
	ocamlfind install $(NAME) META $(INSTALL)

clean:
	ocamlbuild -clean
