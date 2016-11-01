all:	jscl.js repl-node.js repl-web.js

clean:
	-rm -f jscl.js tests.js repl-node.js repl-web.js

jscl.js:	$(shell find . -name \*.lisp)
	./make.sh

repl-node.js:	$(shell find . -name \*.lisp)
	./make.sh

repl-web.js:	$(shell find . -name \*.lisp)
	./make.sh

test:	jscl.js tests.js
	./run-tests.sh
