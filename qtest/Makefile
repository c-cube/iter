
QTEST_PREAMBLE=''
DONTTEST=../src/sequenceLabels.ml ../src/mkflags.ml
QTESTABLE=$(filter-out $(DONTTEST), \
	$(wildcard ../src/*.ml) \
	$(wildcard ../src/*.mli) \
	)

qtest-gen:
	@rm run_qtest.ml || true
	@if which qtest > /dev/null ; then \
		qtest extract --preamble $(QTEST_PREAMBLE) \
			-o run_qtest.ml \
			$(QTESTABLE) 2> /dev/null ; \
	else touch run_qtest.ml ; \
	fi

.PHONY: qtest-gen
