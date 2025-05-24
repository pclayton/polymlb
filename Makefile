PREFIX    ?= /usr/local
MANPREFIX ?= $(PREFIX)/share
BINDIR    ?= $(PREFIX)/bin
LIBDIR    ?= $(PREFIX)/lib
MANDIR    ?= $(MANPREFIX)/man

SML_LIB   ?= $(LIBDIR)/polymlb
POLYC     ?= polyc
INSTALL   ?= install

SRC != find src/bin src/lib -name '*.sml' -o -name '*.sig' -o -name '*.fun'
MLB != find src/lib -name '*.mlb'

all: polymlb

polymlb: $(SRC)
	SML_LIB=$(SML_LIB) $(POLYC) -o $@ src/bin/build.sml

sml_lib: polymlb
	$(eval SML_LIB != ./polymlb -sml-lib)

test:
	$(MAKE) -C test all

install: all sml_lib
	$(INSTALL) -m 755 -d $(DESTDIR)$(BINDIR)
	$(INSTALL) -m 755 -d $(DESTDIR)$(SML_LIB)
	$(INSTALL) -m 755 -d $(DESTDIR)$(MANDIR)/man1
	$(INSTALL) -m 755 polymlb   $(DESTDIR)$(BINDIR)
	cp -RL lib/*                $(DESTDIR)$(SML_LIB)
	$(INSTALL) -m 644 polymlb.1 $(DESTDIR)$(MANDIR)/man1

$(MLB):
	sed \
		-e '/mlb$$/s:^\([[:space:]]*\).*/:\1:g' \
		-e '/\(sml\|sig\|fun\)$$/s:^\([[:space:]]*\):\1$$(ROOT)$(@D:src/lib%=%)/:g' \
		< $@ > src/millet/$(@F)

millet-stubs: $(MLB)

clean:
	rm -f polymlb

test-clean:
	$(MAKE) -C test clean

.PHONY: $(MLB) all clean install millet-stubs sml_lib test
