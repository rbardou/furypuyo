##########################################################################
# Copyright (c) 2009, Romain BARDOU                                      #
# All rights reserved.                                                   #
#                                                                        #
# Redistribution and  use in  source and binary  forms, with  or without #
# modification, are permitted provided that the following conditions are #
# met:                                                                   #
#                                                                        #
# * Redistributions  of  source code  must  retain  the above  copyright #
#   notice, this list of conditions and the following disclaimer.        #
# * Redistributions in  binary form  must reproduce the  above copyright #
#   notice, this list of conditions  and the following disclaimer in the #
#   documentation and/or other materials provided with the distribution. #
# * Neither the name of Fury Puyo nor  the names of its contributors may #
#   be used  to endorse or  promote products derived  from this software #
#   without specific prior written permission.                           #
#                                                                        #
# THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS #
# "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT #
# LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR #
# A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT #
# OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, #
# SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT #
# LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, #
# DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY #
# THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT #
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE #
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   #
##########################################################################

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

dist: furypuyo server
	darcs dist -d furypuyo-`./furypuyo -version`
	@echo "\nPLEASE READ HOWTO-dist.txt"

distbin: furypuyo server
	DIR=furypuyo-`./furypuyo -version`-bin; \
	darcs dist -d $$DIR; \
	mkdir $$DIR; \
	cp furypuyo furypuyo_srv $$DIR; \
	gunzip $$DIR.tar.gz; \
	tar fr $$DIR.tar $$DIR/furypuyo $$DIR/furypuyo_srv; \
	gzip $$DIR.tar; \
	rm $$DIR/furypuyo $$DIR/furypuyo_srv; \
	rmdir $$DIR

wc:
	ocamlwc `darcs query manifest | grep "\.ml"`

.PHONY: furypuyo doc clean distclean dist-clean dist all world server distbin convert