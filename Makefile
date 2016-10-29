all:	jscl.js

jscl.js:	$(shell find . -name \*.lisp)
	./make.sh

test:	jscl.js tests.js
	./run-tests.sh
