open Printf
module C = Docker.Container

let () =
  Common.install_image "debian" ~tag:"latest";
  let c = C.create "debian:latest" ["bash"; "-s"] ~open_stdin:true in
  C.start c;
  let e = C.Exec.create c ["ls"; "-lp"; "/"] in
  let st = C.Exec.start e in
  (* fprintf (Docker.Stream.out st) "ls -l /home/\n%!"; *)
  let s = Docker.Stream.read_all st in
  Docker.Container.stop c;
  Docker.Container.rm c;
  let identify (ty, s) = match ty with
    | Docker.Stream.Stdout -> "out> " ^ s
    | Docker.Stream.Stderr -> "err> " ^ s in
  printf "Exec in the container returned:\n%s\n"
         (String.concat "\n" (List.map identify s))

