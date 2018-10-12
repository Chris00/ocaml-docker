open Printf

let show_stream () =
  let cmd = ["ls"; "-l"] in
  let c = Docker.Container.create "debian:latest" cmd in
  Docker.Container.start c;
  let st = Docker.Container.attach c `Stream ~stdout:true in
  let a = Docker.Stream.read_all st in
  (try Docker.Container.stop c
   with Docker.Failure(_, msg) -> printf "Docker.Failure: %s\n" msg);
  Docker.Container.rm c;
  printf "%S in the container returned (stream):\n> %s\n"
         (String.concat " " cmd) (String.concat "> " (List.map snd a))

let show_logs () =
  let cmd = ["ls"; "-l"] in
  let c = Docker.Container.create "debian:latest" cmd in
  Docker.Container.start c;
  Unix.sleep 1;
  let s = Docker.Container.attach c ~stdout:true `Logs in
  let a = Docker.Stream.read_all s in
  (try Docker.Container.stop c
   with Docker.Failure(_, msg) -> printf "Docker.Failure: %s\n" msg);
  Docker.Container.rm c;
  printf "%S in the container returned (logs):\n> %s\n"
         (String.concat " " cmd) (String.concat "> " (List.map snd a))

let () =
  let logs = ref false in
  let args = [
      "--logs", Arg.Set logs, " use Docker.Container.attach ~logs";
    ] in
  Arg.parse (Arg.align args) (fun _ -> raise(Arg.Bad "no anonymous argument"))
            "ls <options>";
  Common.install_image "debian" ~tag:"latest";
  if !logs then show_logs ()
  else show_stream ()
