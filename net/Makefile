OCAMLBUILD := ocamlbuild -no-links -Is bin -tag debug
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default: tests

tests:
	ln -fs ../bin bin
	$(OCAMLBUILD) test1/client.byte
	$(OCAMLBUILD) test1/server.byte
	$(OCAMLBUILD) test2/client.byte
	$(OCAMLBUILD) test2/server.byte

clean:
	rm -rf _build
	rm bin

.PHONY: default clean test2