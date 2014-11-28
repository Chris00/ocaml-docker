open Printf

let show_stream () =
  let cmd = ["ls"; "-l"] in
  let c = Docker.Container.create "ubuntu:latest" cmd in
  Docker.Container.start c;
  let st = Docker.Container.attach c ~stdout:true ~stream:true in
  let _, s = Docker.Stream.read st ~timeout:5. in
  Docker.Container.stop c;
  Docker.Container.rm c;
  printf "%S in the container returned (stream):\n%s\n"
         (String.concat " " cmd) s

let show_logs () =
  let cmd = ["ls"; "-l"] in
  let c = Docker.Container.create "ubuntu:latest" cmd in
  Docker.Container.start c;
  Unix.sleep 1;
  let s = Docker.Container.attach c ~stdout:true ~logs:true in
  let a = Docker.Stream.read_all s in
  Docker.Container.stop c;
  Docker.Container.rm c;
  printf "%S in the container returned (logs):\n> %s\n"
         (String.concat " " cmd) (String.concat "\n> " (List.map snd a))

let () =
  let logs = ref false in
  let args = [
      "--logs", Arg.Set logs, " use Docker.Container.attach ~logs";
    ] in
  Arg.parse (Arg.align args) (fun _ -> raise(Arg.Bad "no anonymous argument"))
            "ls <options>";
  if !logs then show_logs ()
  else show_stream ()
