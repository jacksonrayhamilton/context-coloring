all:
	exit

test:
	node_modules/.bin/mocha
	emacs -batch -l ert -l test/context-coloring-test.el -f ert-run-tests-batch-and-exit

benchjs:
	node_modules/.bin/matcha

benchel:
	emacs -Q -L . -l context-coloring -l benchmark/scenarios.el

.PHONY: all test benchmark
