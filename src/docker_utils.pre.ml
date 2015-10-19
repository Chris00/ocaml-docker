# 2 "src/docker_utils.pre.ml"

module Int = struct
  let max i j = if (i: int) > j then i else j
end

module Buffer = struct
  include Buffer

  let safe_chars_for_query =
    let a = Array.make 256 false in
    let safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
                0123456789_.-~!$'()*,:@/?" in
    for i = 0 to String.length safe - 1 do
      a.(Char.code safe.[i]) <- true
    done;
    a

  let rec add_pct_encoded_scan buf s start curr len =
  if curr >= len then
    add_substring buf s start (curr - start)
  else
    let c = Char.code s.[curr] in
    if safe_chars_for_query.(c) then
      add_pct_encoded_scan buf s start (curr + 1) len
    else (
      if curr > start then add_substring buf s start (curr - start);
      add_string buf (Printf.sprintf "%%%02X" c);
      add_pct_encoded_scan buf s (curr + 1) (curr + 1) len
    )

  let add_pct_encoded buf s =
    add_pct_encoded_scan buf s 0 0 (String.length s)

  (* Url query encode.  (Avoid to depend on external librarues for
     this and insert it directly into the buffer instead of going
     through a string.)  Inspired by [Uri]. *)
  let add_encoded_query buf = function
    | [] -> ()
    | (k0, v0) :: query ->
       add_string buf "?";
       add_pct_encoded buf k0;
       add_char buf '=';
       add_pct_encoded buf v0;
       let encode (k, v) =
         add_char buf '&';
         add_pct_encoded buf k;
         add_char buf '=';
         add_pct_encoded buf v in
       List.iter encode query


  (* BEGIN COMPATIBILITY *)
  (* Compatibility with pre-Bytes versions of OCaml *)
  let add_subbytes = add_substring
  let to_bytes = contents
  (* END COMPATIBILITY *)
end
