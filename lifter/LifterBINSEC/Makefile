OCAML = ocamlbuild
OCAMLFLAGS = -use-ocamlfind

TARGET = BINSEC

BUILDDIR = build
SRCDIR   = src

.PHONY: all copy clean

all: copy

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

copy: $(BUILDDIR)
	$(OCAML) $(OCAMLFLAGS) -I $(SRCDIR) -r -build-dir $(BUILDDIR) main.native
	cp $(BUILDDIR)/$(SRCDIR)/main.native $(BUILDDIR)/Lifter$(TARGET)

clean:
	rm -rf $(BUILDDIR)
