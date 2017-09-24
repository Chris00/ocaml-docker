(*                                                         -*-tuareg-*- *)

(* Host and User Agent to send to Docker. *)
let http11_header =
  "HTTP/1.1\r\n\
   Host:\r\n\
   User-Agent: OCaml-Docker/%%VERSION%% (%%SYSTEM%%)\r\n"
