open Printf
module C = Docker.Container

let rec seq a b =
  if a > b then []
  else a :: seq (a + 1) b

(* Express an ordered sequence of integers as an union of intervals. *)
type range = Single of int | Range of int * int

let rec ranges (x0: int) = function
  | x :: (y :: _ as tl) ->
     if y = x + 1 then ranges x0 tl
     else (if x = x0 then Single x0 else Range(x0, x)) :: ranges y tl
  | [x] -> [if x = x0 then Single x0 else Range(x0, x)]
  | [] -> []

let ranges l = match l with
  | [] -> []
  | x0 :: _ -> ranges x0 l

let ranges_to_string l =
  let to_string = function
    | Single i -> sprintf "%d" i
    | Range(i1, i2) -> sprintf "%d-%d" i1 i2 in
  String.concat "," (List.map to_string l)

let () =
  Common.install_image "debian" ~tag:"latest";
  let c = C.create "debian:latest" ["bash"; "-s"] ~open_stdin:true in
  C.start c;
  (* Check that sequences of integers created by `seq` in the
     container are correctly read on the output stream of the
     container.  Show incorrect output (collapsing intervals). *)
  let check n =
    assert(n >= 2);
    let e = C.Exec.create c ["seq"; string_of_int n] ~stdin:true in
    let st = C.Exec.start e in
    let s = Docker.Stream.read_all st in
    let buf = Buffer.create (n * truncate(log(float n))) in
    List.iter (function
                | (Docker.Stream.Stdout, s) -> Buffer.add_string buf s
                | (Docker.Stream.Stderr, s) -> failwith("STDERR: " ^ s)
              ) s;
    let out = Str.split (Str.regexp "[ \r\n]+") (Buffer.contents buf) in
    let out = List.map int_of_string out in
    if out <> seq 1 n then (
      printf "n = %d: output = %s\n" n (ranges_to_string (ranges out));
    ) in
  Random.self_init();
  for _i = 1 to 100 do
    check (2 + Random.int 100000)
  done;
  Docker.Container.stop c;
  Docker.Container.rm c
