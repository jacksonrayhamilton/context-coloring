EMACS = emacs
DEPENDENCIES = libraries/ert-async.el libraries/js2-mode.el

all: uncompile compile test

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

uncompile:
	rm -f *.elc libraries/*.elc

clean: uncompile
	rm -f ${DEPENDENCIES}

${DEPENDENCIES}:
	${EMACS} -Q -batch \
	-l scripts/download-dependencies.el \
	-f download-dependencies

test: ${DEPENDENCIES}
	${EMACS} -Q -batch \
	-L . \
	-L libraries \
	-l ert \
	-l ert-async \
	-l context-coloring \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench compile uncompile clean test
