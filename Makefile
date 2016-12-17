EMACS ?= emacs
CASK ?= EMACS=${EMACS} cask
DEPENDENCIES = .cask/
SOURCE_FILES = \
	context-coloring.el \
	context-coloring-javascript.el \
	context-coloring-emacs-lisp.el

all: uncompile compile test

bench: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q \
	-L . \
	-l context-coloring \
	-l context-coloring-benchmark \
	-f context-coloring-benchmark-run

compile: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q -batch \
	-L . \
	-f batch-byte-compile ${SOURCE_FILES}

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
	-l context-coloring-coverage \
	-f context-coloring-coverage-ci-init \
	-l context-coloring-test \
	-f ert-run-tests-batch-and-exit

cover: ${DEPENDENCIES}
	${CASK} exec ${EMACS} -Q -batch \
	-L . \
	-l ert \
	-l context-coloring-coverage \
	-f context-coloring-coverage-local-init \
	-l context-coloring-test \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench compile uncompile clean test cover
