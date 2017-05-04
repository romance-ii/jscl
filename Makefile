# Makefile for JSCL
# Most interesting targets: all, test, doc, clean
all:	jscl.js tests.js jscl-repl repl-web.js

BRANCH=$(shell git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ \1/')

# Eventually, this should build with other compilers; so it's good to start
# building the infrastructure for that time, even if the code is far from
# ready for it.
LISP ?= sbcl


# Defaults for many Lisps. Override below
LISPFLAGS=
LISPLOAD=--load # trailing space
LISPEVAL=--eval # trailing space

ifeq ($(LISP),sbcl)
LISPFLAGS=--non-interactive
endif

ifeq ($(LISP),ccl) # Clozure
LISPFLAGS=--batch
endif

ifeq ($(LISP),ecl)
LISPFLAGS=-q
LISPLOAD=-load # trailing space
LISPEVAL=-eval # trailing space
endif

ifeq ($(LISP),clisp)
LISPFLAGS=-on-error exit -ansi -q
LISPLOAD= #empty
LISPEVAL= #empty
endif

ifeq ($(LISP),alisp) # Allegro CL
LISPLOAD=-L # trailing space
LISPEVAL=-e # trailing space
endif

ifeq ($(LISP),gcl)
LISPFLAGS=-batch
LISPLOAD=-load # trailing space
LISPEVAL=-eval # trailing space
endif

ifeq ($(BRANCH),master)
ALT_BRANCH=/$(BRANCH)
else
ALT_BRANCH=
endif


ALL_LISP=$(shell find . -name \*.lisp -and -not -name .\*)

# This relies  upon a rigid formatting  of the JSON file,  which is also
# used when building JSCL.
VERSION=$(shell grep "version" package.json|cut -d '"' -f4)$(ALT_BRANCH)

clean:
	-rm -f jscl.js tests.js jscl-repl repl-web.js
	-find . -name \*.fasl -or -name \*.fas -or -name \*.lx64fas \
		-exec rm -f {} \;
	-(cd ansi-test; git reset --hard; git clean -f -d)
	-rm -r doc/*
	-mkdir -p doc

jscl.js:	$(ALL_LISP)
	$(LISP) $(LISPFLAGS) $(LISPLOAD)jscl.lisp \
		$(LISPEVAL)'(jscl/bootstrap::load-jscl)' \
		$(LISPEVAL)'(jscl/bootstrap::bootstrap-core t)' 2>&1 |tee jscl.js.build.log

tests.js:	$(ALL_LISP)
	$(LISP) $(LISPFLAGS) $(LISPLOAD)jscl.lisp \
		$(LISPEVAL)'(jscl/bootstrap::load-jscl)' \
		$(LISPEVAL)'(jscl/bootstrap::bootstrap-core)' \
		$(LISPEVAL)'(jscl/bootstrap::compile-test-suite)' 2>&1 |tee tests.js.build.log

jscl-repl:	$(ALL_LISP)
	$(LISP) $(LISPFLAGS) $(LISPLOAD)jscl.lisp \
		$(LISPEVAL)'(jscl/bootstrap::load-jscl)' \
		$(LISPEVAL)'(jscl/bootstrap::bootstrap)' \
		$(LISPEVAL)'(jscl/bootstrap::compile-node-repl)' 2>&1 |tee jscl-repl.build.log

repl-web.js:	$(ALL_LISP)
	$(LISP) $(LISPFLAGS) $(LISPLOAD)jscl.lisp \
		$(LISPEVAL)'(jscl/bootstrap::bootstrap)' \
		$(LISPEVAL)'(jscl/bootstrap::compile-web-repl)' 2>&1 |tee repl-web.js.build.log

test:	jscl.js tests.js
		$(shell find tests ansi-test \
			-\( -name \*.lisp -or -name \*.lsp -\) \
			-and -not -name .\*) \
		.ansi-patched
	$(LISP) $(LISPFLAGS) $(LISPLOAD)jscl.lisp \
		$(LISPEVAL)'(jscl/bootstrap::load-jscl)' \
		$(LISPEVAL)'(jscl/bootstrap::bootstrap-core)' \
		$(LISPEVAL)'(jscl/bootstrap::run-tests-in-host)' 2>&1 |tee test.log
	node tests.js

doc:	doc/jscl.pdf doc/jscl.html.d/index.html

doc/jscl.texi:	$(ALL_LISP) doc-intro.texi doc-conclusion.texi
	$(LISP) $(LISPFLAGS) \
		$(LISPEVAL)'(ql:quickload :net.didierverna.declt)' \
		$(LISPEVAL)'(ql:quickload :alexandria)' \
		$(LISPLOAD)'write-docs.lisp' \
		$(LISPEVAL)'(jscl/doc:write-docs)' 2>&1 | tee doc.build.log

doc/jscl.html.d/index.html:	doc/jscl.texi
	cp doc.css doc/
	cd doc; makeinfo -o jscl.html.d/ \
		--html --css-include=doc.css \
		--split=node jscl.texi

doc/jscl.ps:	doc/jscl.texi
	cd doc; makeinfo --ps -o jscl.ps jscl.texi

doc/jscl.pdf:	doc/jscl.texi
	cd doc; makeinfo --pdf -o jscl.pdf jscl.texi

doc/jscl.txt:	doc/jscl.texi
	cd doc; makeinfo --plaintext -o jscl.txt jscl.texi

doc/jscl.info:	doc/jscl.texi
	cd doc; makeinfo -o jscl.info jscl.texi


.ansi-patched:	ansi-test.patch
	-patch --forward --backup -d ansi-test < ansi-test.patch
	touch .ansi-patched

TAGS:	$(shell find . -name \*.lisp)
	ctags --languages=lisp -e -R -f TAGS

