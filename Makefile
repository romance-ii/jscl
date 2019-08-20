all:	jscl.min.js

jscl.min.js:	jscl.js
	closure-compiler --create_source_map jscl.map \
		$(< ../build/closure-compiler.opts) \
		--js $< \
		--js_output_file $@	

jscl.js: $(find . -name \*.lisp)	
	./make.sh

test:	jscl.js
	./run-tests.sh
