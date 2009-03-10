OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default:
	$(OCAMLBUILD) main.native furypuyo.docdir/index.html
	ln -f -s _build/main.native furypuyo
	ln -f -s _build/furypuyo.docdir doc

clean:
	rm -rf _build
	rm -f furypuyo doc

distclean dist-clean: clean
	rm -f *~

.PHONY: default clean distclean dist-clean