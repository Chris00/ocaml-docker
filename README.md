OCaml client for Docker Remote API
==================================

This library provides an OCaml client for
[Docker Remote API](https://docs.docker.com/reference/api/docker_remote_api/).

Note that the OPAM and ocamlfind packages are called `docker-api` (to
distinguish them from other packages related to Docker) but the OCaml
module is named `Docker` (because the code reads better with that name
IMHO).


Compilation & installation
--------------------------

The easier way to install this library is to use
[OPAM](http://opam.ocaml.org/).  Just type:

    opam install docker-api

This library depends on `ocamlfind >= 1.5.5`, `uri`, and `yojson`.
For the development version, you also need `oasis >= 0.4`.

To compile, just type `make` and then `make install` to install it using
`ocamlfind`.


Testing
-------

If you compile using `make`, the tests will be built (symbolic links
to the executables will be present at the root of this project).  In
order to run them, make sure that the latest Debian image is installed
— if not, simply issue `docker pull debian:latest` in a shell.
