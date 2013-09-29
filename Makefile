EMACS ?= emacs
CASK ?= cask

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = epl.el epl-util.el \
	epl-legacy.el epl-package-desc.el \
	package-legacy.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile: $(OBJECTS)

.PHONY: clean
clean:
	rm -rf $(OBJECTS)

# Setup file dependencies
epl.elc: epl-util.elc epl-legacy.elc epl-package-desc.elc

epl-legacy.elc: epl-util.elc package-legacy.elc

epl-package-desc.elc: epl-util.elc

%.elc : %.el
	$(CASK) exec $(EMACS) -Q --batch -L . -f batch-byte-compile $<
