(lang dune 2.8)
(generate_opam_files true)

(name docker-api)

(source (github Chris00/ocaml-docker))
(license ISC)
(authors "Christophe Troestler <Christophe.Troestler@umons.ac.be>")
(maintainers "Christophe Troestler <Christophe.Troestler@umons.ac.be>")

(package
 (name docker-api)
 (synopsis "Binding to the Docker Remote API")
 (description "Control Docker <https://www.docker.com/> containers using the remote API.")
 (documentation "https://Chris00.github.io/ocaml-docker/doc")
 (depends
  (ocaml (>= 4.03))
  base-bytes
  base-unix
  (yojson (>= 1.6.0))))
