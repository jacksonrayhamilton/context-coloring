all: clean install compile test

bench: benchel benchjs

benchel:
	emacs -Q -L . \
	-l context-coloring \
	-l benchmark/context-coloring-benchmark \
	-f context-coloring-benchmark-run

benchjs:
	node_modules/.bin/matcha

compile:
	emacs -Q -batch -f batch-byte-compile *.el

clean:
	rm -f *.log benchmark/*.log *.elc

install:
	npm install

test: testel testjs

testel:
	emacs -Q -batch -L . \
	-l ert \
	-l context-coloring \
	-l test/context-coloring-test.el \
	-f ert-run-tests-batch-and-exit

testjs:
	node_modules/.bin/mocha

.PHONY: all bench benchel benchjs compile clean install test testel testjs
