open Printf
module C = Docker.Container

let () =
  (* When one tries to run a non-existing command, the call should
     fail with a clear error message. *)
  try
    let c = C.create "alpine:latest" ["/bin/bash"] ~open_stdin:true in
    C.start c;
    printf "Running...\n";
    C.stop c
  with Docker.Failure _ as e ->
    printf "Raised %s\n" (Printexc.to_string e)
