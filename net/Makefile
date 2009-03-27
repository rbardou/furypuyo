OCAMLBUILD := ocamlbuild -no-links
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default: lib test doc

lib:
	$(OCAMLBUILD) network.cma

test:
	$(OCAMLBUILD) -I test client.native server.native
	ln -f -s ../_build/test/client.native test/client
	ln -f -s ../_build/test/server.native test/server

doc:
	$(OCAMLBUILD) network.docdir/index.html

.PHONY: default lib test