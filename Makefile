EMACS = emacs

all: clean compile test

bench:
	${EMACS} -Q \
	-L . \
	-L libraries \
	-l context-coloring \
	-l benchmark/context-coloring-benchmark \
	-f context-coloring-benchmark-run

compile:
	${EMACS} -Q -batch \
	-L libraries \
	-f batch-byte-compile *.el libraries/*.el

clean:
	rm -f *.elc libraries/*.elc

test:
	${EMACS} -Q -batch \
	-L . \
	-L libraries \
	-l ert \
	-l ert-async \
	-l context-coloring \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench compile clean test
