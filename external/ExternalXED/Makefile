BUILDDIR = build
TARGET = $(BUILDDIR)/ExternalXED

all: $(TARGET)

$(BUILDDIR):
	mkdir -p $@

$(TARGET): xed.c $(BUILDDIR)
	$(CC) -Wall -o $@ $< -lxed

clean:
	rm -rf $(BUILDDIR)

.PHONY: all clean
