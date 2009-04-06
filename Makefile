OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default: furypuyo

all world: furypuyo doc

furypuyo:
	$(OCAMLBUILD) -I bin -I net main.native
	ln -f -s _build/main.native furypuyo

convert:
	$(OCAMLBUILD) -I bin convert.native
	ln -f -s _build/convert.native convert

doc:
	$(OCAMLBUILD) -I bin -I net furypuyo.docdir/index.html
	$(OCAMLBUILD) -I bin -I net net/network.docdir/index.html
	ln -f -s _build/furypuyo.docdir doc
	ln -f -s _build/net/network.docdir docnet

clean:
	rm -rf _build
	rm -f furypuyo doc

nettest:
	$(OCAMLBUILD) -I bin -I net -I net/test client.native server.native
	ln -f -s ../../_build/net/test/client.native net/test/client
	ln -f -s ../../_build/net/test/server.native net/test/server

distclean dist-clean: clean
	rm -f *~

dist: furypuyo
	darcs dist -d furypuyo-`./furypuyo -version`

.PHONY: furypuyo doc clean distclean dist-clean dist all world