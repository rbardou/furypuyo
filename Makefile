OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default all world: furypuyo doc

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

.PHONY: furypuyo doc clean distclean dist-clean dist all world