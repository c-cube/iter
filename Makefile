
NAME = sequence
DOC = sequence.docdir/index.html
TARGETS = sequence.cma sequence.cmxa sequence.cmxs sequence.cmi sequence.a
LIB = $(addprefix _build/, $(TARGETS))
INSTALL = $(LIB) sequence.mli

bin:
	ocamlbuild $(TARGETS) $(DOC)

doc: bin
	mkdir -p man/man3/
	ocamlfind ocamldoc -I _build/ sequence.ml sequence.mli -man -d man/man3/

install_file:
	@rm sequence.install || true
	@echo 'doc: [' >> sequence.install
	@for m in $(wildcard sequence.docdir/*.html) ; do \
		echo "  \"?$${m}\"" >> sequence.install; \
	done
	@echo ']' >> sequence.install
	@echo 'man: [' >> sequence.install
	@for m in $(wildcard man/man3/[A-Z]*.3o) ; do \
		echo "  \"?$${m}\"" >> sequence.install; \
	done
	@echo ']' >> sequence.install

all: bin doc install_file

benchs: all
	ocamlbuild -use-ocamlfind -pkg bench -pkg benchmark -pkg unix \
		bench/benchs.native bench/simple_bench.native \
		bench/bench_persistent.native

tests:
	ocamlbuild -use-ocamlfind -pkg oUnit tests/run_tests.native

install: all doc
	ocamlfind install $(NAME) META $(INSTALL)

push_doc: all doc
	scp -r sequence.docdir/* cedeela.fr:~/simon/root/software/sequence/

clean:
	ocamlbuild -clean

.PHONY: all clean tests benchs push_doc
