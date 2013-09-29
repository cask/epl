EMACS ?= emacs
CASK ?= cask

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = epl.el \
	epl-legacy.el epl-package-desc.el \
	package-legacy.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile: $(OBJECTS)

.PHONY: clean
clean:
	rm -rf $(OBJECTS)

%.elc : %.el
	$(CASK) exec $(EMACS) -Q --batch -L . -f batch-byte-compile $<
