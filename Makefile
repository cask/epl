EMACS ?= emacs
CASK ?= cask
COMPILE = ${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile

all: test

compile:
	${COMPILE} epl.el
	${COMPILE} epl-legacy.el
	${COMPILE} epl-desc.el
	${MAKE} clean-elc

clean-elc:
	rm -f epl.elc epl-legacy.elc epl-desc.elc

.PHONY:	compile
