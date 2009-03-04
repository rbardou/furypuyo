OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default:
	rm -f pouyou
	$(OCAMLBUILD) main.native pouyou.docdir/index.html
	ln -s _build/main.native pouyou

.PHONY: default