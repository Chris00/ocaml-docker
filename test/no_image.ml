open Printf
module C = Docker.Container

let () =
  (* When one tries to run a non-existing image, the call should fail
     with a clear error message. *)
  let c = C.create "nonexisting" ["bash"; "-s"] ~open_stdin:true in
  C.start c;
  printf "Running...\n";
  C.stop c
