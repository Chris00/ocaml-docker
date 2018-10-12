open Printf

let install_image ?tag img =
  try Docker.Image.(create (from_image img ?tag))
  with Docker.Error(_, msg) ->
    (* Continuous Integration do not usually have docker installed so
       connecting to the daemon will fail.  Thus, just print a message. *)
    printf "%s: Error: could not install the image %S because %S\n"
      (Filename.basename Sys.argv.(0)) img msg;
    exit 0

