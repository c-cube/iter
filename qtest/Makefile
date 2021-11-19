
QTEST_PREAMBLE='open Iter_shims_;; '
DONTTEST=../src/iterLabels.ml ../src/mkshims.ml ../src/bigarray/mkshims.ml
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard ../src/*.ml) \
	$(wildcard ../src/*.mli) \
	)

qtest-gen:
	@rm run_qtest.ml 2>/dev/null || true
	@if which qtest > /dev/null ; then \
		qtest extract --preamble $(QTEST_PREAMBLE) \
			-o run_qtest.ml \
			$(QTESTABLE) 2> /dev/null ; \
	else touch run_qtest.ml ; \
	fi

.PHONY: qtest-gen
