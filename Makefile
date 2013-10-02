EMACS = emacs
CASK = cask

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

# Export the used EMACS to recipe environments
export EMACS

# Keep these files sorted alphabetically, to compile them in the same order as
# package.el would do
SRCS = epl.el package-legacy.el
OBJECTS = $(SRCS:.el=.elc)

.PHONY: compile
compile: $(SRCS)
	$(CASK) exec $(EMACS) -Q --batch -L . -f batch-byte-compile $(SRCS)

.PHONY: clean
clean:
	rm -rf $(OBJECTS)
