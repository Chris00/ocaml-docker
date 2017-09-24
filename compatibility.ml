#load "str.cma";;

(* Utils
 ***********************************************************************)

let major, minor =
  Scanf.sscanf Sys.ocaml_version "%i.%i" (fun a b -> a,b)

let input_file ?(path=".") ?(comments=true) fname =
  let fh = open_in (Filename.concat path fname) in
  let buf = Buffer.create 2048 in
  try
    while true do
      let l = input_line fh in (* or exn *)
      Buffer.add_string buf l;
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    close_in fh;
    Buffer.contents buf

let output_file ?(path=".") fname ~content =
  let fh = open_out (Filename.concat path fname) in
  output_string fh content;
  close_out fh

(* Compatibility
 ***********************************************************************)

let () =
  let fname_in = "docker_utils.pre.ml" in
  let fname_out = "docker_utils.ml" in
  if major >= 4 && minor >= 2 then (
  (* Comment out the compatibility code. *)
    let s = input_file fname_in in
    let s = Str.global_replace (Str.regexp "BEGIN COMPATIBILITY \\*)") "" s in
    let s = Str.global_replace (Str.regexp "(\\* END COMPATIBILITY") "" s in
    output_file fname_out s
  )
  else (
    (* Just copy the file. *)
    output_file fname_out (input_file fname_in)
  )

(* Docker config
 ***********************************************************************)

let () =
  let fname_in = "docker_config.pre.ml" in
  let fname_out = "docker_config.ml" in
  let s = input_file fname_in in
  let s = Str.global_replace (Str.regexp_string "%%SYSTEM%%") Sys.os_type s in
  let s = Str.global_replace (Str.regexp_string "%%VERSION%%") "dev" s in
  output_file fname_out s

;;
