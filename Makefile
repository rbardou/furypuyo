OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default: furypuyo

all world: furypuyo doc

furypuyo:
	$(OCAMLBUILD) main.native
	ln -f -s _build/main.native furypuyo

convert:
	$(OCAMLBUILD) convert.native
	ln -f -s _build/convert.native convert

doc:
	$(OCAMLBUILD) furypuyo.docdir/index.html
	$(OCAMLBUILD) -I net net/network.docdir/index.html
	ln -f -s _build/furypuyo.docdir doc
	ln -f -s _build/net/network.docdir docnet

clean:
	rm -rf _build
	rm -f furypuyo doc

distclean dist-clean: clean
	rm -f *~

dist: furypuyo
	darcs dist -d furypuyo-`./furypuyo -version`

.PHONY: furypuyo doc clean distclean dist-clean dist all world