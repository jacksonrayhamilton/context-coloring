all:
	exit

test:
	node_modules/.bin/mocha
	emacs -batch -l ert -l test/context-coloring-test.el -f ert-run-tests-batch-and-exit

benchmark:
	node_modules/.bin/matcha

.PHONY: all test benchmark
