open Printf

let () =
  let c = Docker.Container.create "alpine:latest" ["sleep"; "2"]
            ~name:"some_email_somewhere" in
  printf "Created container id: %s\n%!" c;
  Docker.Container.start c;
  let l = Docker.Container.list() in
  Docker.Container.stop c;
  Docker.Container.rm c;
  printf "List containers:\n";
  List.iter (fun c -> printf "id: %s\n" c.Docker.Container.id) l
