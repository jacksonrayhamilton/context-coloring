EMACS = emacs

all: clean compile test

bench:
	${EMACS} -Q \
	-L . \
	-L lib \
	-l context-coloring \
	-l benchmark/context-coloring-benchmark \
	-f context-coloring-benchmark-run

compile:
	${EMACS} -Q -batch \
	-L lib \
	-f batch-byte-compile *.el lib/*.el

clean:
	rm -f *.log benchmark/*.log *.elc lib/*.elc

test:
	${EMACS} -Q -batch \
	-L . \
	-L lib \
	-l ert \
	-l ert-async \
	-l context-coloring \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench compile clean test
