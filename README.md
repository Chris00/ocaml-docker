OCaml client for Docker Remote API
==================================

This library provides an OCaml client for
[Docker Remote API](https://docs.docker.com/reference/api/docker_remote_api/).

Compilation & installation
--------------------------

The easier way to install this library is to use
[OPAM](http://opam.ocaml.org/).  Just type:

    opam install docker

This library depends on `ocamlfind >= 1.5.5`, `uri`, and `yojson`.
For the development version, you also need `oasis >= 0.4`.

To compile, just type `make` and then `make install` to install it using
`ocamlfind`.
