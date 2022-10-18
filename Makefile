
all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

format:
	@dune build @fmt --auto-promote

DUNE_OPTS ?= --profile=release
BENCH_TARGETS= benchs.exe bench_persistent_read.exe bench_persistent.exe

benchs:
	@for i in $(BENCH_TARGETS) ; do \
	  echo "run benchmark $$i" ; \
	  dune exec $(DUNE_OPTS) "src/bench/$$i" ; done

build-benchs:
	@dune build $(DUNE_OPTS) $(addprefix src/bench/, $(BENCH_TARGETS))

bench-persistent:
	@dune exec $(DUNE_OPTS) src/bench/bench_persistent.exe
bench-persistent-read:
	@dune exec $(DUNE_OPTS) src/bench/bench_persistent_read.exe
benchs-iter:
	@dune exec $(DUNE_OPTS) src/bench/benchs.exe

examples:
	dune exec examples/test_sexpr.exe

VERSION=$(shell awk '/^version:/ {print $$2}' iter.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

WATCH ?= @install
watch:
	dune build $(WATCH) -w

.PHONY: benchs tests examples update_next_tag
