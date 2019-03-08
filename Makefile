
all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

BENCH_TARGETS= benchs.exe bench_persistent_read.exe bench_persistent.exe

benchs:
	@for i in $(BENCH_TARGETS) ; do \
	  echo "run benchmark $$i" ; \
	  dune exec "src/bench/$$i" ; done

build-benchs:
	@dune build $(addprefix src/bench/, $(BENCH_TARGETS))

examples:
	dune build examples/test_sexpr.exe

VERSION=$(shell awk '/^version:/ {print $$2}' iter.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make all; \
	done

.PHONY: benchs tests examples update_next_tag push_doc push_stable watch
