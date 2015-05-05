CASK = cask
EMACS = emacs
DEPENDENCIES = .cask/

all: uncompile compile test

bench: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q \
	-L . \
	-l context-coloring \
	-l benchmark/context-coloring-benchmark.el \
	-f context-coloring-benchmark-run

compile: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q -batch \
	-L . \
	-f batch-byte-compile *.el

uncompile:
	rm -f *.elc

clean: uncompile
	rm -rf ${DEPENDENCIES}

${DEPENDENCIES}:
	${CASK}

test: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q -batch \
	-L . \
	-l ert \
	-l ert-async \
	-l test/context-coloring-coverage.el \
	-f context-coloring-coverage-ci-init \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

cover: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q -batch \
	-L . \
	-l ert \
	-l ert-async \
	-l test/context-coloring-coverage.el \
	-f context-coloring-coverage-local-init \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench compile uncompile clean test cover
