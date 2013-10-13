EMACS ?= emacs
CASK ?= cask

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

SRCS = epl.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile: $(SRCS)
	$(CASK) exec $(EMACS) -Q --batch -L . -f batch-byte-compile $(SRCS)

.PHONY: clean
clean:
	rm -rf $(OBJECTS)

.PHONY: test
test: clean
	$(CASK) exec ert-runner
	$(MAKE) compile
	$(CASK) exec ert-runner
	$(MAKE) clean
