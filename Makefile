all: clean install compile test

bench: benchjs benchel

benchjs:
	node_modules/.bin/matcha

benchel:
	emacs -Q -L . \
	-l context-coloring \
	-l benchmark/context-coloring-benchmark \
	-f context-coloring-benchmark-run

compile:
	emacs -Q -batch -f batch-byte-compile *.el

clean:
	rm -f *.log benchmark/*.log *.elc

install:
	npm install

test:
	node_modules/.bin/mocha
	emacs -Q -batch -L . \
	-l ert \
	-l context-coloring \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

.PHONY: all bench benchjs benchel compile clean install test
