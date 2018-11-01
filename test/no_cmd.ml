open Printf
module C = Docker.Container

let () =
  (* When one tries to run a non-existing command, the call should
     fail with a clear error message. *)
  try
    let c = C.create "alpine:latest" ["/bin/bash"] ~open_stdin:true in
    C.start c;
    assert false
  with Docker.Failure _ as e ->
    printf "Raised %s\n%!" (Printexc.to_string e)

let () =
  try
    let c = C.create "alpine:latest" ["/bin/ash"] ~open_stdin:true in
    C.start c;
    let e = C.Exec.create c ["bash"] in
    let stream = C.Exec.start e in
    let ty, s = Docker.Stream.read stream in
    printf "Read %S on %s\n%!" s (match ty with Stdout -> "Stdout"
                                              | Stderr -> "Stderr");
    C.stop c;
    C.rm c;
    (* assert false *)
    printf "Unfortunately, Docker does not indicate when a command is not \
            available.\n"
  with Docker.Failure _ as e ->
    printf "Raised %s\n%!" (Printexc.to_string e)
