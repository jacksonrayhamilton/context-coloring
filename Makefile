all:
	exit

test:
	mocha
	emacs -batch -l ert -l test/context-coloring-test.el -f ert-run-tests-batch-and-exit

.PHONY: all test clean
