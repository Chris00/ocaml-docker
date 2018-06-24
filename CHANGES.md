0.2 2017-10-15
--------------

- Upgrade to API v1.29.
- New signature of `Container.create`.
- Add functions `Container.wait` and `Container.changes`.
- Handle errors `409 Conflict`.
- New exceptions `Docker.Failure` and `Docker.No_such_container`.
- New tests `ls` and `ps`.
- Use [Dune](https://github.com/ocaml/dune) and
  [dune-release](https://github.com/samoht/dune-release).


