open Printf
module C = Docker.Container

let () =
  (* When one tries to run a non-existing image, the call should fail
     with a clear error message. *)
  try
    let _c = C.create "nonexisting" ["bash"; "-s"] ~open_stdin:true in
    assert false
  with Docker.Failure _ ->
    printf "Good, raised exception as expected.\n"
