all:	jscl.js test


clean:
	-rm -f jscl.js tests.js repl-web.js repl-node.js
jscl.js:	$(shell find . -name \*.lisp)
	./make.sh

test:	jscl.js tests.js
	./run-tests.sh

