OCAML = ocamlbuild
OCAMLFLAGS =  -ocamlopt 'ocamlopt -g'
OCAMLLIBS = -package core_kernel \
	-package yojson \
  -package bap

TARGET = BAP

BUILDDIR = build
SRCDIR   = src

.PHONY: all copy clean

all: copy

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

copy: $(BUILDDIR)
	$(OCAML) $(OCAMLFLAGS) $(OCAMLLIBS) -I $(SRCDIR) -r -build-dir $(BUILDDIR) main.native
	cp $(BUILDDIR)/$(SRCDIR)/main.native $(BUILDDIR)/Lifter$(TARGET)

clean:
	rm -rf $(BUILDDIR)
