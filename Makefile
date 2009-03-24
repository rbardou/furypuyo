OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default: furypuyo

all world: furypuyo doc

furypuyo:
	$(OCAMLBUILD) main.native
	ln -f -s _build/main.native furypuyo

doc:
	$(OCAMLBUILD) furypuyo.docdir/index.html
	ln -f -s _build/furypuyo.docdir doc

clean:
	rm -rf _build
	rm -f furypuyo doc

distclean dist-clean: clean
	rm -f *~

dist: furypuyo
	darcs dist -d furypuyo-`./furypuyo -version`

nettest:
	$(OCAMLBUILD) -I nettest client.native server.native
	ln -f -s ../_build/nettest/client.native nettest/client
	ln -f -s ../_build/nettest/server.native nettest/server

.PHONY: furypuyo doc clean distclean dist-clean dist all world nettest