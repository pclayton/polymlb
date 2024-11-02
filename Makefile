PREFIX    ?= /usr/local
MANPREFIX ?= $(PREFIX)/share
BINDIR    ?= $(PREFIX)/bin
LIBDIR    ?= $(PREFIX)/lib
MANDIR    ?= $(MANPREFIX)/man

SML_LIB   ?= $(LIBDIR)/polymlb
POLYC     ?= polyc
INSTALL   ?= install

all: polymlb

polymlb: src/*/*.sml
	SML_LIB=$(SML_LIB) $(POLYC) -o $@ src/bin/build.sml

install: all
	$(eval SML_LIB != ./polymlb -sml-lib)
	$(INSTALL) -m 755 -d $(DESTDIR)$(BINDIR)
	$(INSTALL) -m 755 -d $(DESTDIR)$(SML_LIB)
	$(INSTALL) -m 755 -d $(DESTDIR)$(MANDIR)/man1
	$(INSTALL) -m 755 polymlb   $(DESTDIR)$(BINDIR)
	cp -rL lib/*                 $(DESTDIR)$(SML_LIB)
	$(INSTALL) -m 644 polymlb.1 $(DESTDIR)$(MANDIR)/man1

clean:
	rm -f polymlb

.PHONY: all clean install test
