# This Makefile is intended for developers.  Users simply use dune.

PKGVERSION = $(shell git describe --always --dirty)

all build:
	dune build @install @tests

test runtest:
	dune runtest --force

install uninstall:
	dune $@

doc: all
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' src/docker.mli \
	  > _build/default/src/docker.mli
	dune build @doc

lint:
	@opam lint docker-api.opam

clean:
	dune clean
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg)

.PHONY: all build test runtest clean
