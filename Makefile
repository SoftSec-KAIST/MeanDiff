BUILDDIR = build
SRCDIR = src
LIFTERDIR = lifter
EXTERNALDIR = external

all: meandiff

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

init:
	git submodule init
	git submodule update

lifters: $(BUILDDIR)
	cd $(LIFTERDIR) && $(MAKE) -f Makefile.lifter

external: $(BUILDDIR)
	cd $(EXTERNALDIR) && $(MAKE) -f Makefile.external

meandiff:
	cd $(SRCDIR) && $(MAKE) -f Makefile.src

clean:
	rm -rf $(BUILDDIR)

.PHONY: all init lifters external build
