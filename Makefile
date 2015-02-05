EMACS = emacs
DEPENDENCIES = libraries/ert-async.el libraries/js2-mode.el

all: clean compile test

bench: ${DEPENDENCIES}
	${EMACS} -Q \
	-L . \
	-L libraries \
	-l context-coloring \
	-l benchmark/context-coloring-benchmark \
	-f context-coloring-benchmark-run

compile: ${DEPENDENCIES}
	${EMACS} -Q -batch \
	-L . \
	-L libraries \
	-f batch-byte-compile *.el libraries/*.el

clean:
	rm -f *.elc libraries/*.elc ${DEPENDENCIES}

${DEPENDENCIES}:
	${EMACS} -Q -batch \
	-l scripts/download-dependencies.el

test: ${DEPENDENCIES}
	${EMACS} -Q -batch \
	-L . \
	-L libraries \
	-l ert \
	-l ert-async \
	-l context-coloring \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench compile clean test
