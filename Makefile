
all:
	ocamlbuild tests.native sequence.docdir/index.html

clean:
	ocamlbuild -clean
