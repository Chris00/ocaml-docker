
(* BEGIN COMPATIBILITY *)
(* Compatibility with pre-Bytes versions of OCaml *)

module Buffer = struct
  include Buffer
  let add_subbytes = add_substring
  let to_bytes = contents
end

(* END COMPATIBILITY *)
