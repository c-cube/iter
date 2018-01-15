
all: build test

build:
	jbuilder build @install

test:
	jbuilder runtest --no-buffer

clean:
	jbuilder clean

doc:
	jbuilder build @doc

BENCH_TARGETS=bench_persistent_read.exe bench_persistent.exe

benchs:
	jbuilder build $(addprefix bench/, $(BENCH_TARGETS))

examples:
	jbuilder build examples/test_sexpr.exe

push_doc: all doc
	scp -r sequence.docdir/* cedeela.fr:~/simon/root/software/sequence/

VERSION=$(shell awk '/^version:/ {print $$2}' sequence.opam)

SOURCE=$(addprefix src/, *.ml *.mli invert/*.ml invert/*.mli bigarray/*.ml bigarray/*.mli)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(SOURCE)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(SOURCE)

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make all; \
	done

.PHONY: benchs tests examples update_next_tag push_doc push_stable watch
