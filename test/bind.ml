open Printf
module C = Docker.Container

let () =
  let c = C.create "debian:latest" ["dash"; "-s"] ~open_stdin: true
            ~host:(C.host () ~binds:[C.Mount("_build/", "/tmp/b")]) in
  C.start c;
  ignore(C.Exec.(start (create c ["touch"; "/tmp/b/bind.txt"])));
  let e = C.Exec.create c ["ls"; "-l"; "/tmp/b"] in
  let st = C.Exec.start e in
  let s = Docker.Stream.read_all st in
  Docker.Container.stop c;
  Docker.Container.rm c;
  let identify (ty, s) = match ty with
    | Docker.Stream.Stdout -> "out> " ^ s
    | Docker.Stream.Stderr -> "err> " ^ s in
  printf "Exec in the container returned:\n%s\n"
         (String.concat "\n" (List.map identify s))

