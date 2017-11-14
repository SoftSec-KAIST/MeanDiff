TARGET = PyVEX

BUILDDIR = build
SRCDIR   = src

.PHONY: all copy clean

all: copy

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

copy: $(BUILDDIR)
	cp $(SRCDIR)/PyVEX $(BUILDDIR)/Lifter$(TARGET)
	cp -r $(SRCDIR)/LifterPyVEX-src $(BUILDDIR)/

clean:
	rm -rf $(BUILDDIR)
