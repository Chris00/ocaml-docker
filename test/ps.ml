open Printf
module C = Docker.Container

let () =
  Common.install_image "alpine" ~tag:"latest";
  let cmd = ["sleep"; "0.3"] in
  let c = C.create "alpine:latest" ~name:"waiting_container" cmd in
  printf "Created container id: %s\n%!" c;
  (try C.start c
   with Docker.Failure(_, m) -> printf "Docker.Container.start: %s\n" m);
  let l = C.list() in
  (try C.stop c
   with Docker.Failure(_, m) -> printf "Docker.Container.stop: %s\n" m);
  C.rm c;
  printf "List containers:\n";
  List.iter (fun c -> printf "- id: %s\n  names: %s\n"
                        c.C.id (String.concat ", " c.C.names)) l
