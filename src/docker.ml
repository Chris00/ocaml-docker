module Json = Yojson.Safe

exception Invalid_argument of string
exception Server_error of string * string

let default_addr =
  ref(Unix.ADDR_UNIX "/var/run/docker.sock")

let set_default_addr addr = default_addr := addr

let connect fn_name addr =
  let fd = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  try Unix.connect fd addr;
      fd
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    Unix.close fd;
    raise(Server_error(fn_name, "Cannot connect: socket does not exist"))

(* Return a number < 0 of not found.
   It is ASSUMED that [pos] and [len] delimit a valid substring. *)
let rec index_CRLF (s: Bytes.t) ~pos ~len =
  if len <= 1 then -1 (* Cannot match "\r\n" *)
  else if Bytes.get s pos = '\r' && Bytes.get s (pos+1) = '\n' then pos
  else index_CRLF s ~pos:(pos + 1) ~len:(len - 1)

(* Return the list of header lines and keep in [buf] the additional
   bytes that may have been read. *)
let read_headers fn_name buf fd =
  let headers = ref [] in
  let b = Bytes.create 4096 in
  let continue = ref true in
  while !continue do
    let r = Unix.read fd b 0 4096 in
    if r > 0 then
      (* Split on \r\n *)
      let i = index_CRLF b ~pos:0 ~len:r in
      if i < 0 then
        Buffer.add_subbytes buf b 0 r
      else if i = 0 && Buffer.length buf = 0 then (
        (* End of headers (all previously captured). *)
        Buffer.add_subbytes buf b 0 r;
        continue := false
      )
      else (
        Buffer.add_subbytes buf b 0 i;
        headers := Buffer.contents buf :: !headers;
        Buffer.clear buf;
        (* Capture all possible additional headers in [b]. *)
        let pos = ref (i+2) and len = ref (r - i - 2) in
        let i = ref 0 in
        while (i := index_CRLF b ~pos:!pos ~len:!len;  !i > !pos) do
          let h_len = !i - !pos in
          headers := Bytes.sub_string b !pos h_len  :: !headers;
          pos := !i + 2;
          len := !len - h_len - 2;
        done;
        if !i < 0 then Buffer.add_subbytes buf b !pos !len
        else ( (* !i = !pos, i.e., empty line *)
          Buffer.add_subbytes buf b (!pos + 2) (!len - 2);
          continue := false;
        )
      )
    else continue := false
  done;
  match List.rev !headers with
  | [] ->
     Unix.close fd;
     raise (Server_error(fn_name, "No status sent"))
  | status :: tl ->
     let code =
       try let i1 = String.index status ' ' in
           let i2 = String.index_from status (i1 + 1) ' ' in
           int_of_string(String.sub status (i1 + 1) (i2 - i1 - 1))
       with _ ->
         Unix.close fd;
         raise(Server_error(fn_name, "Incorrect status line: " ^ status)) in
     (* Let the client functions deal with 4xx to have more precise
        messages. *)
     code, tl

(* [read_all buf fd] add to [buf] the content of [fd] until EOI is reached. *)
let read_all buf fd =
  let b = Bytes.create 4096 in
  let continue = ref true in
  while !continue do
    let r = Unix.read fd b 0 4096 in
    if r > 0 then Buffer.add_subbytes buf b 0 r
    else continue := false
  done;
  Buffer.contents buf

let read_response fn_name fd =
  let buf = Buffer.create 4096 in
  let status, h = read_headers fn_name buf fd in
  if status = 204 (* No Content *) || status = 205 then status, h, ""
  else
    let body = read_all buf fd in
    (* In case of error 500, the body may provide an explanation. *)
    if status >= 500 then (
      Unix.close fd;
      raise(Server_error(fn_name, body));
    );
    status, h, body

(* When the command returns a stream, we only attempt to read the
   whole payload in case of error. *)
let deal_with_status_500 fn_name status buf fd =
  if status >= 500 then (
    let body = read_all buf fd in
    Unix.close fd;
    raise(Server_error(fn_name, body));
  )

let get fn_name addr url query =
  let fd = connect fn_name addr in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "GET ";
  Buffer.add_string buf url;
  Buffer.add_string buf "?";
  Buffer.add_string buf (Uri.encoded_of_query query);
  Buffer.add_string buf " HTTP/1.1\r\n\r\n";
  ignore(Unix.write fd (Buffer.to_bytes buf) 0 (Buffer.length buf));
  fd

let response_of_get fn_name addr url query =
  let fd = get fn_name addr url query in
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let r = read_response fn_name fd in
  Unix.close fd;
  r

let post fn_name  addr url query json =
  let fd = connect fn_name addr in
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "POST ";
  Buffer.add_string buf url;
  (match query with
   | [] -> ()
   | _ -> Buffer.add_string buf "?";
         Buffer.add_string buf (Uri.encoded_of_query query));
  Buffer.add_string buf " HTTP/1.1\r\n\
                         Content-Type: application/json\r\n\
                         Content-Length: ";
  (match json with
   | None ->
      Buffer.add_string buf "0\r\n\r\n";
   | Some json ->
      let json = Json.to_string json in
      Buffer.add_string buf (string_of_int (String.length json));
      Buffer.add_string buf "\r\n\r\n";
      Buffer.add_string buf json);
  ignore(Unix.write fd (Buffer.to_bytes buf) 0 (Buffer.length buf));
  fd

let response_of_post fn_name addr url query json =
  let fd = post fn_name addr url query json in
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let r = read_response fn_name fd in
  Unix.close fd;
  r

let delete fn_name addr url query =
  let fd = connect fn_name addr in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "DELETE ";
  Buffer.add_string buf url;
  Buffer.add_string buf "?";
  Buffer.add_string buf (Uri.encoded_of_query query);
  Buffer.add_string buf " HTTP/1.1\r\n\r\n";
  ignore(Unix.write fd (Buffer.to_bytes buf) 0 (Buffer.length buf));
  fd

let status_of_delete fn_name addr url query =
  let fd = delete fn_name addr url query in
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let status, _, _ = read_response fn_name fd in
  Unix.close fd;
  status

(* Generic JSON utilities *)

let string_of_json fn_name = function
  | `String s -> s
  | j -> raise(Server_error(fn_name, "Not a JSON string:" ^ Json.to_string j))

let json_of_strings = function
  | [] -> `Null
  | l -> `List(List.map (fun s -> `String s) l)


(* Stream processing for [attach] and [Exec]. *)

module Stream = struct
  (* Always assume the TTY is set to false.  Thus the stream is
     multiplexed to separate stdout and stderr. *)

  type t = {
      fd: Unix.file_descr; (* Hijacked from the transport *)
      out: out_channel;
      (* Read buffer.  The bytes buf.[i] with [i0 <= i < i1] contain
         the data. *)
      buf: Bytes.t;
      mutable i0: int;
      mutable i1: int;
      (* Partially decoded stream *)
    }

  type kind = Stdout | Stderr
  type read_kind = STDIN | STDOUT | STDERR

  exception Timeout

  (* [buf] is a buffer containing the payload already read. *)
  let create buffer fd =
    let len = Buffer.length buffer in
    let buf = Bytes.create (max len 4096) in
    Buffer.blit buffer 0  buf 0 len;
    { fd;  out = Unix.out_channel_of_descr fd;
      buf;  i0 = 0;  i1 = 0 }

  let out st = st.out

  let shutdown st =
    Unix.shutdown st.fd Unix.SHUTDOWN_SEND

  (* [timeout < 0] means unbounded wait. *)
  let is_ready_for_read fd ~timeout =
    let fds, _, _ = Unix.select [fd] [] [] timeout in
    fds <> []

  let fill st ~timeout =
    if is_ready_for_read st.fd ~timeout then
      (* Append data to the existing one. *)
      let r = Unix.read st.fd st.buf st.i1 (Bytes.length st.buf - st.i1) in
      st.i1 <- st.i1 + r;
      r
    else
      raise Timeout

  (* After a call to this function st.i0 < st.i1 or we have reached
     the end of the stream. *)
  let fill_if_needed st ~timeout =
    if st.i0 >= st.i1 then (
      st.i0 <- 0;
      st.i1 <- 0;
      ignore(fill st ~timeout);
    )

  let rec fill_8bytes_with_timeout st ~timeout =
    if st.i0 + 8 > st.i1 then (
      (* Only if we do not have the required bytes we care about the time. *)
      if timeout < 0. then raise Timeout;
      let t0 = Unix.time () in
      let r = fill st ~timeout in (* or raise Timeout *)
      let timeout = timeout -. (Unix.time () -. t0) in
      if r = 0 then
        (* Nothing to read, bail out with the condition unsatisfied. *)
        if timeout >= 0. then timeout else 0.
      else fill_8bytes_with_timeout st ~timeout
    )
    else
      (* If time is exceeded, allow only for immediate reads *)
      if timeout >= 0. then timeout else 0.

  let rec fill_8bytes_unbounded_wait st =
    if st.i0 + 8 > st.i1 then (
      let r = fill st ~timeout:(-1.) in (* < 0, thus unbounded wait *)
      if r > 0 then fill_8bytes_unbounded_wait st
    )

  let fill_8bytes st ~timeout =
    if st.i0 + 8 >= Bytes.length st.buf then (
      (* Move the data to the beginning to have at least 8 bytes after i0 *)
      let len = st.i1 - st.i0 in
      Bytes.blit st.buf st.i0  st.buf 0 len;
      st.i1 <- len;
      st.i0 <- 0;
    );
    if timeout < 0. then (fill_8bytes_unbounded_wait st; timeout)
    else fill_8bytes_with_timeout st ~timeout

  let read_header st ~timeout =
    let timeout = fill_8bytes st ~timeout in
    (* Check whether we succeeded to fill. *)
    if st.i0 + 8 > st.i1 then
      (* The end of the stream is reached.  If we have no bytes at all
         in the pipeline, consider that the stream is empty. *)
      if st.i0 >= st.i1 then (STDOUT, 0, timeout)
      else raise(Server_error("Docker.Stream", "truncated header"))
    else (
      let typ = match Bytes.get st.buf st.i0 with
        | '\000' -> STDIN
        | '\001' -> STDOUT
        | '\002' -> STDERR
        | _ -> raise(Server_error("Docker.Stream.read",
                                 "invalid STREAM_TYPE")) in
      let size1 = Char.code(Bytes.get st.buf (st.i0 + 4)) in
      let size2 = Char.code(Bytes.get st.buf (st.i0 + 5)) in
      let size3 = Char.code(Bytes.get st.buf (st.i0 + 6)) in
      let size4 = Char.code(Bytes.get st.buf (st.i0 + 7)) in
      let len = size1 lsl 24 + size2 lsl 16 + size3 lsl 8 + size4 in
      if Sys.word_size = 32
         && (size1 lsr 7 = 1 || len > Sys.max_string_length) then
        failwith "Docker.Stream.read: payload exceeds max string length \
                  (32 bits)";
      st.i0 <- st.i0 + 8; (* 8 bytes processed *)
      typ, len, timeout
    )

  (* Reads [len] bytes and store them in [b] starting at position [ofs]. *)
  let rec really_read_unbounded_wait st b ofs len =
    if len > 0 then (
      fill_if_needed st ~timeout:(-1.);
      let buf_len = st.i1 - st.i0 in
      if len <= buf_len then (
        Bytes.blit st.buf st.i0  b ofs len;
        st.i0 <- st.i0 + len;
      )
      else ( (* len > buf_len *)
        Bytes.blit st.buf st.i0  b ofs buf_len;
        st.i0 <- st.i0 + buf_len;
        really_read_unbounded_wait st b (ofs + buf_len) (len - buf_len)
      )
    )

  let rec really_read_with_timeout st b ofs len ~timeout =
    if len > 0 then (
      let t0 = Unix.time() in
      fill_if_needed st ~timeout;
      let buf_len = st.i1 - st.i0 in
      if len <= buf_len then (
        Bytes.blit st.buf st.i0  b ofs len;
        st.i0 <- st.i0 + len;
      )
      else ( (* len > buf_len *)
        Bytes.blit st.buf st.i0  b ofs buf_len;
        st.i0 <- st.i0 + buf_len;
        let timeout = timeout -. (Unix.time () -. t0) in
        really_read_with_timeout st b (ofs + buf_len) (len - buf_len) ~timeout
      )
    )

  let rec read ?(timeout= -1.) st =
    let typ, len, timeout1 = read_header st ~timeout in
    let payload = Bytes.create len in
    if timeout1 >= 0. then
      really_read_with_timeout st payload 0 len ~timeout:timeout1
    else really_read_unbounded_wait st payload 0 len;
    match typ with
    | STDOUT -> Stdout, Bytes.unsafe_to_string payload
    | STDERR -> Stderr, Bytes.unsafe_to_string payload
    | STDIN -> read ~timeout st (* skip without decreasing the timeout *)

  let read_all st =
    let l = ref [] in
    let continue = ref true in
    while !continue do
      let (_, b) as r = read st in
      if String.length b = 0 then continue := false
      else l := r :: !l
    done;
    !l


  let close st =
    close_out st.out (* also closes the underlying file descriptor *)
end


module Container = struct
  type id = string

  type port = { priv: int;  pub: int;  typ: string }

  let port_of_json_assoc l =
    (* No port is a negative integer. *)
    let priv = ref (-1) and pub = ref (-1) and typ = ref "" in
    let update = function
      | ("PrivatePort", `Int i) -> priv := i
      | ("PublicPort", `Int i) -> pub := i
      | ("Type", `String s) -> typ := s
      | _ -> () in
    List.iter update l;
    if !priv < 0 || !pub < 0 || !typ = "" then
      raise(Server_error("Docker.Container.list", "Incorrect port elements"));
    { priv = !priv;  pub = !pub;  typ = !typ }

  let port_of_json = function
    | `Assoc port -> port_of_json_assoc port
    | _ -> raise(Server_error("Docker.Container.list", "Incorrect port"));

  type t = {
      id: id;
      names: string list;
      image: string;
      command: string;
      created: float;
      status: string;
      ports: port list;
      size_rw: int;
      size_root_fs: int;
    }

  let container_of_json (c: Json.json) =
    match c with
    | `Assoc l ->
       let id = ref "" and names = ref [] and image = ref "" in
       let command = ref "" and created = ref 0. and status = ref "" in
       let ports = ref [] and size_rw = ref 0 and size_root_fs = ref 0 in
       let update = function
         | ("Id", `String s) -> id := s
         | ("Names", `List l) ->
            names := List.map (string_of_json "Docker.Container.list") l
         | ("Image", `String s) -> image := s
         | ("Command", `String s) -> command := s
         | ("Created", `Int i) -> created := float i (* same as Unix.time *)
         | ("Status", `String s) -> status := s
         | ("Ports", `List p) -> ports := List.map port_of_json p
         | ("SizeRw", `Int i) -> size_rw := i
         | ("SizeRootFs", `Int i) -> size_root_fs := i
         | _ -> () in
       List.iter update l;
       { id = !id;  names = !names;  image = !image;  command = !command;
         created = !created;  status = !status;  ports = !ports;
         size_rw = !size_rw;  size_root_fs = !size_root_fs }
    | _ -> raise(Server_error("Docker.Container.list",
                             "Invalid container: " ^ Json.to_string c))

  let list ?(addr= !default_addr) ?(all=false) ?limit ?since ?before
           ?(size=false) () =
    let q = if all then ["all", ["1"]] else [] in
    let q = match limit with
      | Some l -> ("limit", [string_of_int l]) :: q
      | None -> q in
    let q = match since with
      | Some id -> ("since", [id]) :: q
      | None -> q in
    let q = match before with
      | Some id -> ("before", [id]) :: q
      | None -> q in
    let q = if size then ("size", ["1"]) :: q else q in
    let status, _, body = response_of_get "Docker.Container.list" addr
                                          "/containers/json" q in
    if status >= 400 then
      raise(Invalid_argument("Docker.Container.list: Bad parameter"));
    match Json.from_string body with
    | `List l -> List.map container_of_json l
    | _ ->
       raise(Server_error("Docker.Container.list",
                          "response not a JSON list: " ^ body))


  let json_of_bind (host_path, container_path, access) =
    (* FIXME: check the paths to not contain ":" *)
    match access with
    | `RO -> `String(host_path ^ ":" ^ container_path ^ ":ro")
    | `RW -> `String(host_path ^ ":" ^ container_path)

  let json_of_binds = function
    | [] -> (`Null: Json.json)
    | binds -> `List(List.map json_of_bind binds)

  let create ?(addr= !default_addr) ?(hostname="") ?(domainname="")
             ?(user="") ?(memory=0) ?(memory_swap=0)
             ?(attach_stdin=false) ?(attach_stdout=true) ?(attach_stderr=true)
             ?(open_stdin=false) ?(stdin_once=false)
             ?(env=[]) ?(workingdir="") ?(networking=false)
             ?(binds=[])
             image cmd =
    let json : Json.json =
      `Assoc [
         ("Hostname", `String hostname);
         ("Domainname", `String domainname);
         ("User", `String user);
         ("Memory", `Int memory);
         ("MemorySwap", `Int memory_swap);
         ("CpuShares", `Int 0); (* TODO *)
         ("Cpuset", `String "");  (* TODO *)
         ("AttachStdin", `Bool attach_stdin);
         ("AttachStdout", `Bool attach_stdout);
         ("AttachStderr", `Bool attach_stderr);
         ("Tty", `Bool false);
         ("OpenStdin", `Bool open_stdin);
         ("StdinOnce", `Bool stdin_once);
         ("Env", json_of_strings env);
         ("Cmd", json_of_strings cmd);
         ("Entrypoint", `Null); (* TODO *)
         ("Image", `String image);
         ("Volumes", `Null);     (* TODO *)
         ("WorkingDir", `String workingdir);
         ("NetworkDisabled", `Bool(not networking));
         ("ExposedPorts", `Null); (* TODO *)
         ("SecurityOpts", `Null); (* TODO *)
         ("HostConfig",
          `Assoc [
             ("Binds", json_of_binds binds);
             ("Links", `Null);              (* TODO *)
             ("LxcConf", `List []);           (* TODO *)
             ("PortBindings", `Assoc []);      (* TODO *)
             ("PublishAllPorts", `Bool false); (* TODO *)
             ("Privileged", `Bool false);      (* TODO *)
             ("Dns", `Null); (* TODO *)
             ("DnsSearch", `Null);  (* TODO *)
             ("VolumesFrom", `List []);          (* TODO *)
             ("CapAdd", `Null);               (* TODO *)
             ("CapDrop", `Null);              (* TODO *)
             ("RestartPolicy",
              `Assoc [("Name", `String "");
                      ("MaximumRetryCount", `Int 0)]);  (* TODO *)
             ("NetworkMode", `String "bridge");  (* TODO *)
             ("Devices", `List []);              (* TODO *)
           ]);
       ] in
    let status, _, body =
      response_of_post "Docker.Container.create" addr
                       "/containers/create" [] (Some json) in
    if status >= 406 then
      raise(Invalid_argument("Docker.Container.create: \
                              Impossible to attach (container not running)"))
    else if status >= 400 then
      raise(Invalid_argument("Docker.Container.create: No such container"));
    (* Extract ID *)
    match Json.from_string body with
    | `Assoc l ->
       (try string_of_json "Docker.Containers.create" (List.assoc "Id" l)
        with Not_found ->
          raise(Server_error("Docker.Containers.create", "No ID returned")))
    | _ ->
       raise(Server_error("Docker.Container.create",
                          "Response must be an association list: " ^ body ))


  let start ?(addr= !default_addr) ?(binds=[])
            id =
    (* FIXME: may want to check that [id] does not contain special chars *)
    let json : Json.json =
      `Assoc [
         ("Binds", json_of_binds binds);
         ("Links", `Null);              (* TODO *)
         ("LxcConf", `List []);         (* TODO *)
         ("PortBindings", `Assoc []);   (* TODO *)
         ("PublishAllPorts", `Bool false); (* TODO *)
         ("Privileged", `Bool false);      (* TODO *)
         ("Dns", `Null); (* TODO *)
         ("DnsSearch", `Null);  (* TODO *)
         ("VolumesFrom", `List []);          (* TODO *)
         ("CapAdd", `Null);               (* TODO *)
         ("CapDrop", `Null);              (* TODO *)
         ("RestartPolicy",
          `Assoc [("Name", `String "");
                  ("MaximumRetryCount", `Int 0)]);  (* TODO *)
         ("NetworkMode", `String "bridge");  (* TODO *)
         ("Devices", `List []);              (* TODO *)
       ] in
    let path = "/containers/" ^ id ^ "/start" in
    let status, h, body = response_of_post "Docker.Container.start" addr
                                           path [] (Some json) in
    if status >= 400 then
      raise(Invalid_argument("Docker.Container.start: No such container"))
    (* FIXME: do we want to react on 304 – container already started ? *)


  let stop ?(addr= !default_addr) ?wait id =
    let q = match wait with None -> []
                          | Some t -> ["t", [string_of_int t]] in
    let path = "/containers/" ^ id ^ "/stop" in
    let status, _, _ = response_of_post "Docker.Container.stop" addr
                                        path q None in
    if status >= 400 then
      raise(Invalid_argument("Docker.Container.stop: No such container"))
    (* FIXME: do we want to react on 304 – container already stopped ? *)

  let rm ?(addr= !default_addr) ?(volumes=false) ?(force=false) id =
    let q = ["v", [string_of_bool volumes];
             "force", [string_of_bool force]] in
    let path = "/containers/" ^ id in
    let status = status_of_delete "Docker.Container.rm" addr path q in
    if status >= 404 then
      raise(Invalid_argument("Docker.Container.stop: No such container"))
    else if status >= 400 then
      raise(Invalid_argument("Docker.Container.stop: Bad parameter"))


  let attach ?(addr= !default_addr) ?(logs=false) ?(stream=false)
             ?(stdin=false) ?(stdout=false) ?(stderr=false) id =
    let q = ["logs", [string_of_bool logs];
             "stream", [string_of_bool stream];
             "stdin", [string_of_bool stdin];
             "stdout", [string_of_bool stdout];
             "stderr", [string_of_bool stderr] ] in
    let path = "/containers/" ^ id ^ "/attach" in
    let fd = post "Docker.Containers.attach" addr path q None in
    let buf = Buffer.create 4096 in
    let status, h = read_headers "Docker.Containers.attach" buf fd in
    deal_with_status_500 "Docker.Containers.attach" status buf fd;
    if status >= 400 then (
      Unix.close fd;
      let msg =
        if status >= 404 then "Docker.Containers.attach: no such container"
        else "Docker.Containers.attach: bad parameter" in
      raise(Invalid_argument msg);
    );
    Stream.create buf fd

  module Exec = struct
    type t = string (* exec ID *)

    let create ?(addr= !default_addr) ?(stdin=false) ?(stdout=true)
               ?(stderr=true) container cmd =
      let json =
        `Assoc ["AttachStdin", `Bool stdin;
                "AttachStdout", `Bool stdout;
                "AttachStderr", `Bool stderr;
                "Tty", `Bool false;
                "Cmd", json_of_strings cmd;
                "Container", `String container ] in
      let path = "/containers/" ^ container ^ "/exec" in
      let status, _, body = response_of_post "Docker.Container.Exec.create"
                                             addr path [] (Some json) in
      if status >= 400 then (
        let msg = "Docker.Container.Exec.create: no such container" in
        raise(Invalid_argument msg);
      );
      (* Extract ID *)
      match Json.from_string body with
      | `Assoc l ->
         (try string_of_json "Docker.Containers.create" (List.assoc "Id" l)
          with _ -> raise(Server_error("Docker.Containers.Exec.create",
                                      "No ID returned")))
      | _ ->
         raise(Server_error("Docker.Container.Exec.create",
                            "Response must be an association list: " ^ body ))


    let start ?(addr= !default_addr) exec_id =
      let json = `Assoc ["Detach", `String "false";
                         "Tty", `String "false" ] in
      let path = "/exec/" ^ exec_id ^ "/start" in
      let fd = post "Docker.Containers.Exec.start" addr path [] (Some json) in
      let buf = Buffer.create 4096 in
      let status, h = read_headers "Docker.Containers.Exec.start" buf fd in
      deal_with_status_500 "Docker.Containers.Exec.start" status buf fd;
      if status >= 400 then (
        Unix.close fd;
        let msg = "Docker.Container.Exec.start: no such exec instance" in
        raise(Invalid_argument msg);
      );
      Stream.create buf fd


    (* TODO: Exec Resize *)
    ;;
  end
end

module Images = struct
  type id = string
  type t = {
      id: id;
      created: float;
      size: int;
      virtual_size: int;
      tags: string list;
    }

  let image_of_json (img: Json.json) =
    match img with
    | `Assoc l ->
       let id = ref "" and created = ref nan and size = ref 0 in
       let virtual_size = ref 0 and tags = ref [] in
       let update = function
         | ("RepoTags", `List l) ->
            tags := List.map (string_of_json "Docker.Images.list") l
         | ("Id", `String s) -> id := s
         | ("Created", `Int i) -> created := float i
         | ("Size", `Int i) -> size := i
         | ("VirtualSize", `Int i) -> virtual_size := i
         | _ -> () in
       List.iter update l;
       { id = !id;  created = !created;  size = !size;
         virtual_size = !virtual_size;  tags = !tags }
    | _ -> raise(Server_error("Docker.Images.list",
                             "Invalid image: " ^ Json.to_string img))

  let list ?(addr= !default_addr) ?(all=false) () =
    let q = ["all", [string_of_bool all]] in
    let _, _, body = response_of_get "Docker.Images.list" addr
                                     "/images/json" q in
    match Json.from_string body with
    | `List l -> List.map image_of_json l
    | _ ->
       raise(Server_error("Docker.Images.list",
                          "Response must be a JSON list: " ^ body))


end

type version = { api_version: string;
                 version: string;
                 git_commit: string;
                 go_version: string }

let version ?(addr= !default_addr) () =
  let _, _, body = response_of_get "Docker.version" addr "/version" [] in
  match Json.from_string body with
  | `Assoc l ->
     let api_version = ref "" and version = ref "" in
     let git_commit = ref "" and go_version = ref "" in
     let update = function
       | ("ApiVersion", `String s) -> api_version := s
       | ("Version", `String s) -> version := s
       | ("GitCommit", `String s) -> git_commit := s
       | ("GoVersion", `String s) -> go_version := s
       | _ -> () in
     List.iter update l;
     { api_version = !api_version;  version = !version;
       git_commit = !git_commit;    go_version = !go_version }
  | _ -> raise(Server_error("Docker.version",
                           "Response must be a JSON association list: " ^ body))



;;
