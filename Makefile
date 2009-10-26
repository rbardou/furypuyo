OCAMLBUILD := ocamlbuild -no-links -Is bin,net,server
ifeq ($(TERM), dumb)
	OCAMLBUILD := $(OCAMLBUILD) -classic-display
endif

default: furypuyo server

all world: furypuyo server doc

furypuyo:
	$(OCAMLBUILD) main.native
	ln -f -s _build/main.native furypuyo

convert:
	$(OCAMLBUILD) convert.native
	ln -f -s _build/convert.native convert

doc:
	$(OCAMLBUILD) furypuyo.docdir/index.html
	$(OCAMLBUILD) net/network.docdir/index.html
	ln -f -s _build/furypuyo.docdir doc
	ln -f -s _build/net/network.docdir docnet

server:
	$(OCAMLBUILD) server.native
	ln -f -s _build/server/server.native furypuyo_srv

clean:
	rm -rf _build
	rm -f furypuyo doc

distclean dist-clean: clean
	rm -f *~

dist: furypuyo
	darcs dist -d furypuyo-`./furypuyo -version`

distbin: furypuyo
	DIR=furypuyo-`./furypuyo -version`-bin; \
	darcs dist -d $$DIR; \
	mkdir $$DIR; \
	cp furypuyo $$DIR; \
	gunzip $$DIR.tar.gz; \
	tar fr $$DIR.tar $$DIR/furypuyo; \
	gzip $$DIR.tar; \
	rm $$DIR/furypuyo; \
	rmdir $$DIR

.PHONY: furypuyo doc clean distclean dist-clean dist all world server distbin convert