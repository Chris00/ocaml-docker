open Docker_utils
module Json = Yojson.Safe

exception No_such_container of string
exception Failure of string * string
exception Invalid_argument of string
exception Server_error of string
exception Error of string * string

let default_addr =
  ref(Unix.ADDR_UNIX "/var/run/docker.sock")

let set_default_addr addr = default_addr := addr
(* FIXME: When Unix.ADDR_UNIX, check that the file exists?? *)

let connect fn_name addr =
  let fd = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  try Unix.connect fd addr;
      fd
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    Unix.close fd;
    raise(Error(fn_name, "Cannot connect: socket does not exist"))

(* Return a number < 0 if not found.
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
     raise (Error(fn_name, "No status sent"))
  | status :: tl ->
     let code =
       try let i1 = String.index status ' ' in
           let i2 = String.index_from status (i1 + 1) ' ' in
           int_of_string(String.sub status (i1 + 1) (i2 - i1 - 1))
       with _ ->
         Unix.close fd;
         raise(Error(fn_name, "Incorrect status line: " ^ status)) in
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
  if status = 204 (* No Content *) || status = 205 (* Reset Content *) then
    status, h, ""
  else
    (* FIXME: Use Content-Length header if exists ? *)
    let body = read_all buf fd in
    (* In case of error 500, the body may provide an explanation... but
       it may also stall the whole computation so do not read it. *)
    if status >= 500 then (
      Unix.close fd;
      raise(Server_error fn_name);
    );
    status, h, body

(* When the command returns a stream, we only attempt to read the
   whole payload in case of error. *)
let deal_with_status_500 fn_name status fd =
  if status >= 500 then (
    Unix.close fd;
    raise(Server_error fn_name);
  )

let[@inline] send_buffer fn_name addr buf =
  let fd = connect fn_name addr in
  ignore(Unix.write fd (Buffer.to_bytes buf) 0 (Buffer.length buf));
  fd

let get fn_name addr url query =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "GET /v1.29";
  Buffer.add_string buf url;
  Buffer.add_encoded_query buf query;
  Buffer.add_string buf Docker_config.http11_header;
  Buffer.add_string buf "\r\n";
  send_buffer fn_name addr buf

let response_of_get fn_name addr url query =
  let fd = get fn_name addr url query in
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let r = read_response fn_name fd in
  Unix.close fd;
  r

(* Return a buffer containing the beginning of the header, excluding
   Content-* headers. *)
let[@inline] post_header url query =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "POST /v1.29";
  Buffer.add_string buf url;
  Buffer.add_encoded_query buf query;
  Buffer.add_string buf Docker_config.http11_header;
  buf

let post fn_name  addr url query json =
  let buf = post_header url query in
  Buffer.add_string buf "Content-Type: application/json\r\n\
                         Content-Length: ";
  (match json with
   | None ->
      Buffer.add_string buf "0\r\n\r\n";
   | Some json ->
      let json = Json.to_string json in
      Buffer.add_string buf (string_of_int (String.length json));
      Buffer.add_string buf "\r\n\r\n";
      Buffer.add_string buf json);
  send_buffer fn_name addr buf

let response_of_post fn_name addr url query json =
  let fd = post fn_name addr url query json in
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let r = read_response fn_name fd in
  Unix.close fd;
  r

let status_response_of_post fn_name addr url query json ~id =
  let status, _, _ = response_of_post fn_name addr url query json in
  if status >= 404 then raise(No_such_container id);
  status

let unit_response_of_post fn_name addr url query json ~id =
  ignore(status_response_of_post fn_name addr url query json ~id)

let delete fn_name addr url query =
  let fd = connect fn_name addr in
  let buf = Buffer.create 256 in
  Buffer.add_string buf "DELETE ";
  Buffer.add_string buf url;
  Buffer.add_encoded_query buf query;
  Buffer.add_string buf Docker_config.http11_header;
  Buffer.add_string buf "\r\n";
  ignore(Unix.write fd (Buffer.to_bytes buf) 0 (Buffer.length buf));
  fd

let response_of_delete fn_name addr url query =
  let fd = delete fn_name addr url query in
  Unix.shutdown fd Unix.SHUTDOWN_SEND;
  let r = read_response fn_name fd in
  Unix.close fd;
  r

(* Generic JSON utilities *)

let string_of_json fn_name = function
  | `String s -> s
  | j -> raise(Error(fn_name, "Not a JSON string:" ^ Json.to_string j))

let json_of_strings = function
  | [] -> `Null
  | l -> `List(List.map (fun s -> `String s) l)

let message_of_body body =
  match Json.from_string body with
  | `Assoc l ->
     (try (match List.assoc "message" l with
           | `String m -> m
           | j -> Json.to_string j)
      with Not_found -> body)
  | _ -> body

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
    let buf = Bytes.create (Int.max len 4096) in
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

  let fill_unbounded_wait st =
    (* Append data to the existing one. *)
    let r = Unix.read st.fd st.buf st.i1 (Bytes.length st.buf - st.i1) in
    st.i1 <- st.i1 + r;
    r

  let fill st ~timeout =
    if is_ready_for_read st.fd ~timeout then fill_unbounded_wait st
    else raise Timeout

  (* After a call to this function st.i0 < st.i1 or we have reached
     the end of the stream. *)
  let fill_if_needed st ~timeout =
    if st.i0 >= st.i1 then (
      st.i0 <- 0;
      st.i1 <- 0;
      ignore(if timeout < 0. then fill_unbounded_wait st
             else fill st ~timeout);
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
      let r = fill_unbounded_wait st in
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
      else raise(Error("Docker.Stream", "truncated header"))
    else (
      let typ = match Bytes.get st.buf st.i0 with
        | '\000' -> STDIN
        | '\001' -> STDOUT
        | '\002' -> STDERR
        | _ -> raise(Error("Docker.Stream.read", "invalid STREAM_TYPE")) in
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
      if len = 0 then
        raise(Error("Docker.Stream.read", "Payload with 0 length"));
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
    List.rev !l


  let close st =
    close_out st.out (* also closes the underlying file descriptor *)
end


module Container = struct
  type id = string
  type id_or_name = string

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
      raise(Error("Docker.Container.list", "Incorrect port elements"));
    { priv = !priv;  pub = !pub;  typ = !typ }

  let port_of_json = function
    | `Assoc port -> port_of_json_assoc port
    | _ -> raise(Error("Docker.Container.list", "Incorrect port"));

  type t = {
      id: id;
      names: string list;
      image: string;
      command: string;
      created: float;
      status: string;
      ports: port list;
      size_rw: int option;
      size_root_fs: int option;
    }

  let container_of_json (c: Json.json) =
    match c with
    | `Assoc l ->
       let id = ref "" and names = ref [] and image = ref "" in
       let command = ref "" and created = ref 0. and status = ref "" in
       let ports = ref [] and size_rw = ref (-1) and size_root_fs = ref(-1) in
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
         size_rw = if !size_rw < 0 then None else Some !size_rw;
         size_root_fs = if !size_root_fs < 0 then None else Some !size_root_fs }
    | _ -> raise(Error("Docker.Container.list",
                       "Invalid container: " ^ Json.to_string c))

  let json_of_health = function
    | `Starting -> `String "starting"
    | `Healthy -> `String "healthy"
    | `Unhealthy -> `String "unhealthy"
    | `None -> `String "none"

  let json_of_status = function
      | `Created -> `String "created"
      | `Restarting -> `String "restarting"
      | `Running -> `String "running"
      | `Removing -> `String "removing"
      | `Paused -> `String "paused"
      | `Exited -> `String "exited"
      | `Dead -> `String "dead"

  let list ?(addr= !default_addr) ?(all=false) ?limit ?(size=false)
        ?before ?exited ?health ?name ?since ?status ?volume () =
    let q = if all then ["all", "1"] else [] in
    let q = match limit with
      | Some l -> ("limit", string_of_int l) :: q
      | None -> q in
    let q = if size then ("size", "1") :: q else q in
    let filters = [] in
    let filters = match before with
      | Some id -> ("before", `List[`String id]) :: filters
      | None -> filters in
    let filters = match exited with
      | Some i -> ("exited", `List(List.map (fun i -> `Int i) i)) :: filters
      | None -> filters in
    let filters = match health with
      | Some h -> ("health", `List(List.map json_of_health h)) :: filters
      | None -> filters in
    let filters = match name with
      | Some n -> ("name", `List(List.map (fun n -> `String n) n)) :: filters
      | None -> filters in
    let filters = match since with
      | Some id -> ("since", `List[`String id]) :: filters
      | None -> filters in
    let filters = match status with
      | Some s -> ("status", `List(List.map json_of_status s)) :: filters
      | None -> filters in
    let filters = match volume with
      | Some id -> ("volume", `List[`String id]) :: filters
      | None -> filters in
    let q = match filters with
      | _ :: _ -> ("filters", Json.to_string (`Assoc filters)) :: q
      | [] -> q in
    let status, _, body = response_of_get "Docker.Container.list" addr
                                          "/containers/json" q in
    if status >= 400 then
      raise(Invalid_argument("Docker.Container.list"));
    match Json.from_string body with
    | `List l -> List.map container_of_json l
    | _ ->
       raise(Error("Docker.Container.list",
                   "response not a JSON list: " ^ body))


  type bind =
    | Vol of string
    | Mount of string * string
    | Mount_ro of string * string

  let absolute_path path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd()) path
    else path

  let json_of_bind = function
    (* FIXME: check the paths to not contain ":" *)
    | Vol v -> `String v
    | Mount(host_path, container_path) ->
       `String(absolute_path host_path ^ ":" ^ container_path)
    | Mount_ro(host_path, container_path) ->
       `String(absolute_path host_path ^ ":" ^ container_path ^ ":ro")

  let json_of_binds = function
    | [] -> (`Null: Json.json)
    | binds -> `List(List.map json_of_bind binds)

  let disallowed_chars_for_name =
    let a = Array.make 256 true in
    let safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz\
                0123456789_-" in
    for i = 0 to String.length safe - 1 do
      a.(Char.code safe.[i]) <- false
    done;
    a

  let disallowed_char_for_name c =
    Array.unsafe_get disallowed_chars_for_name (Char.code c)

  (* Checks that the name is composed of allowed chars only. *)
  let name_is_not_allowed name =
    let len = String.length name in
    if len = 0 then true
    else if len = 1 then disallowed_char_for_name (String.unsafe_get name 0)
    else (* len >= 2 *)
      try
        let c0 = String.unsafe_get name 0 in
        if disallowed_char_for_name c0 && c0 <> '/' then raise Exit;
        for i = 0 to String.length name - 1 do
          if disallowed_char_for_name(String.unsafe_get name i) then raise Exit
        done;
        false
      with Exit -> true

  type host_config = {
      cpu_shares : int;
      memory : int;
      cgroup_parent : string;
      blk_io_weight : int;
      (* blk_io_weight_device : throttle_device; *)
      (* blk_io_device_read_bps : throttle_device; *)
      (* blk_io_device_write_bps : throttle_device; *)
      (* blk_io_device_read_iops : throttle_device; *)
      (* blk_io_device_write_iops : throttle_device; *)
      cpu_period : int;
      (* cpu_quota : int64; *)
      (* cpu_realtime_period : int64; *)
      (* cpu_realtime_runtime : int64; *)
      (* cpuset_cpus : string; *)
      (* cpuset_mems : string; *)
      (* devices : device_mapping; *)
      (* device_cgroup_rules : string; *)
      (* disk_quota : int64; *)
      (* kernel_memory : int; *)
      (* memory_reservation : int; *)
      memory_swap : int;
      (* memory_swappiness : int; *)
      (* nano_cpus : int; *)
      (* oom_kill_disable : bool; *)
      (* pids_limit : int; *)
      (* ulimits : ulimits; *)
      (* cpu_count : int *)
      (* cpu_percent : int; *)
      (* io_maximum_iops : int; *)
      (* io_maximum_bandwidth : int; *)
      binds : bind list;
      network_mode : string;
      policy : [ `None | `Auto_remove | `Restart_always
               | `Restart_unless_stopped | `Restart_on_failure of int];
    }

  let host ?(cpu_shares=0) ?(memory=0) ?(cgroup_parent="")
        ?(blk_io_weight= -1) ?(cpu_period=0) ?(memory_swap= -1)
        ?(binds=[]) ?(network_mode = "bridge") ?(policy = `None) () =
    { cpu_shares;  memory;  cgroup_parent;
      blk_io_weight;
      cpu_period;
      memory_swap;
      binds;
      network_mode;
      policy;
    }

  let default_host = host()

  let restart_policy name count =
    ("RestartPolicy", `Assoc [("Name", `String name);
                              ("MaximumRetryCount", `Int count)])


  let create ?(addr= !default_addr) ?(hostname="") ?(domainname="")
        ?(user="") ?(stdin=false) ?(stdout=true) ?(stderr=true)
        ?(open_stdin=false) ?(stdin_once=false)
        ?(env=[]) ?(workingdir="") ?(networking=false)
        ?(host=default_host)
        ?name
        image cmd =
    (*** Host Config *)
    let host_config =
      if host.cpu_shares > 0 then [("CpuShares", `Int host.cpu_shares)]
      else [] in
    (* Ensure that "You must use this with memory and make the swap
       value larger than memory". *)
    let memory, memory_swap =
      if host.memory_swap <= 0 then (Int.max host.memory 0, -1)
      else if host.memory <= 0 (* = not set *) then
        (host.memory_swap, host.memory_swap)
      else (host.memory, Int.max host.memory host.memory_swap) in
    let host_config = ("Memory", `Int memory)
                      :: ("MemorySwap", `Int memory_swap)
                      :: ("CgroupParent", `String host.cgroup_parent)
                      :: host_config in
    let host_config =
      if 0 <= host.blk_io_weight && host.blk_io_weight <= 1000 then
        ("BlkioWeight", `Int host.blk_io_weight) :: host_config
      else host_config in
    (* BlkioWeightDevice *)
    (* BlkioDeviceReadBps *)
    (* BlkioDeviceWriteBps *)
    (* BlkioDeviceReadIOps *)
    (* BlkioDeviceWriteIOps *)
    let host_config = if host.cpu_period > 0 then
                        ("CpuPeriod", `Int host.cpu_period) :: host_config
                      else host_config in
    (* CpuQuota *)
    (* CpuRealtimePeriod *)
    (* CpuRealtimeRuntime *)
    (* CpusetCpus *)
    (* CpusetMems *)
    (* ("Devices", `List []) *)
    (* DeviceCgroupRules *)
    (* DiskQuota *)
    (* KernelMemory *)
    (* MemoryReservation *)
    (* MemorySwappiness *)
    (* NanoCPUs *)
    (* OomKillDisable *)
    (* PidsLimit *)
    (* Ulimits *)
    (* CpuCount *)
    (* CpuPercent *)
    (* IOMaximumIOps — Windows *)
    (* IOMaximumBandwidth *)
    let host_config = ("Binds", json_of_binds host.binds) :: host_config in
    (* ContainerIDFile *)
    (* LogConfig *)
    let host_config = if host.network_mode <> "" then
                        ("NetworkMode", `String "bridge") :: host_config
                      else host_config in
    (* PortBindings *)
    let host_config = match host.policy with
      | `Auto_remove -> ("AutoRemove", `Bool true) :: host_config
      | `Restart_always -> restart_policy "always" 0 :: host_config
      | `Restart_unless_stopped -> restart_policy "unless-stopped" 0
                                   :: host_config
      | `Restart_on_failure n ->
         if n > 0 then restart_policy "on-failure" n :: host_config
         else host_config
      | `None -> host_config in
    (* VolumeDriver *)
    (* VolumesFrom *)
    (* Mounts *)
    (* CapAdd *)
    (* CapDrop *)
    (* Dns *)
    (* DnsOptions *)
    (* DnsSearch *)
    (* ExtraHosts *)
    (* GroupAdd *)
    (* IpcMode *)
    (* Cgroup *)
    (* Links *)
    (* OomScoreAdj *)
    (* PidMode *)
    (* Privileged *)
    (* PublishAllPorts *)
    (* ReadonlyRootfs *)
    (* SecurityOpt *)
    (* StorageOpt *)
    (* Tmpfs *)
    (* UTSMode *)
    (* UsernsMode *)
    (* ShmSize *)
    (* Sysctls *)
    (* Runtime *)
    (* ConsoleSize — Windows *)
    (* Isolation — Windows *)
    (*** Main payload *)
    let json : Json.json =
      `Assoc [
          ("Hostname", `String hostname);
          ("Domainname", `String domainname);
          ("User", `String user);
          ("AttachStdin", `Bool stdin);
          ("AttachStdout", `Bool stdout);
          ("AttachStderr", `Bool stderr);
          ("ExposedPorts", `Null); (* TODO *)
          ("Tty", `Bool false); (* WARNING: see also [attach]. *)
          ("OpenStdin", `Bool open_stdin);
          ("StdinOnce", `Bool stdin_once);
          ("Env", json_of_strings env);
          ("Cmd", json_of_strings cmd);
          (* Healthcheck *)
          (* ArgsEscaped: only for Windows; do not set *)
          ("Image", `String image);
          ("Volumes", `Null);     (* TODO *)
          ("WorkingDir", `String workingdir);
          ("Entrypoint", `Null); (* TODO *)
          ("NetworkDisabled", `Bool(not networking));
          (* MacAddress *)
          (* OnBuild *)
          (* Labels *)
          (* StopSignal *)
          (* StopTimeout *)
          (* Shell *)
          ("HostConfig", `Assoc host_config);
          (* NetworkingConfig *)
        ] in
    let query_params = match name with
      | Some name ->
         if name_is_not_allowed name then
           invalid_arg(Printf.sprintf "Docker.Container.create: container \
                                       name %S is not allowed" name);
         [("name", name)]
      | None -> [] in
    let status, _, body =
      response_of_post "Docker.Container.create" addr
                       "/containers/create" query_params (Some json) in
    if status >= 409 then
      raise(Failure("Docker.Container.create", body))
    else if status >= 406 then
      raise(Failure("Docker.Container.create",
                    "Impossible to attach (container not running)"))
    else if status >= 400 then (
      (* Try to extract the container ID. *)
      match message_of_body body with
      | m -> (try let i = String.index m ':' in
                 let id = String.sub m (i + 2) (String.length m - i - 2) in
                 raise(No_such_container id)
              with _ ->
                raise(Failure("Docker.Container.create", m)))
      | exception Yojson.Json_error _ ->
         raise(Server_error (Printf.sprintf "body %S contains no message" body))
    );
    (* Extract ID *)
    match Json.from_string body with
    | `Assoc l ->
       (try string_of_json "Docker.Containers.create" (List.assoc "Id" l)
        with Not_found ->
          raise(Error("Docker.Containers.create", "No ID returned")))
    | _ ->
       raise(Error("Docker.Container.create",
                   "Response must be an association list: " ^ body ))

  let change_of_json = function
    | `Assoc c ->
       (try let path = List.assoc "Path" c in
            let kind = match List.assoc "Kind" c with
              | `Int 0 -> `Modified
              | `Int 1 -> `Added
              | `Int 2 -> `Deleted
              | j -> raise(Error("Docker.Container.changes",
                                 "Invalid kind:" ^ Json.to_string j)) in
            (string_of_json "Docker.Container.changes" path, kind)
        with Not_found -> raise(Error("Docker.Container.changes",
                                      "Invalid change object")))
    | j -> raise(Error("Docker.Container.changes",
                       "object expected, got: " ^ Json.to_string j))

  let changes ?(addr= !default_addr) id =
    let path = "/containers/" ^ id ^ "/changes" in
    let _, _, body = response_of_get "Docker.Container.changes" addr path [] in
    match Json.from_string body with
    | `List l -> List.map change_of_json l
    | _ -> raise(Error("Docker.Container.changes", "Invalid response: " ^ body))


  let start ?(addr= !default_addr) ?(detach_keys="") id =
    (* FIXME: may want to check that [id] does not contain special chars *)
    let q = if detach_keys <> "" then ["detachKeys", detach_keys] else [] in
    let path = "/containers/" ^ id ^ "/start" in
    let status, _, body = response_of_post "Docker.Container.start" addr path q
                            None in
    if status >= 404 then raise(No_such_container id);
    if status >= 400 then
      (* This is an undocumented status that is raised when the
         command asked to run in [create] does not exist. *)
      raise(Failure("Docker.Container.start", message_of_body body));
    if status >= 304 then
      raise(Failure("Docker.Container.start", "Container already started"))


  let stop ?(addr= !default_addr) ?wait id =
    let q = match wait with None -> []
                          | Some t -> ["t", string_of_int t] in
    let path = "/containers/" ^ id ^ "/stop" in
    let status =
      status_response_of_post "Docker.Container.stop" addr path q None ~id in
    if status >= 304 then
      raise(Failure("Docker.Container.stop", "Container already stopped"))

  let wait ?(addr= !default_addr) id =
    let path = "/containers/" ^ id ^ "/wait" in
    let _, _, body = response_of_post "Docker.Container.wait"
                       addr path [] None in
    match Json.from_string body with
    | `Assoc l ->
       (try (match List.assoc "StatusCode" l with
             | `Int s -> s
             | _ -> raise(Error("Docker.Container.wait", "Invalid StatusCode")))
        with Not_found ->
          raise(Error("Docker.Container.wait", "Invalid response: " ^ body)))
    | _ -> raise(Error("Docker.Container.wait", "Invalid response: " ^ body))

  let restart ?(addr= !default_addr) ?wait id =
    let q = match wait with None -> []
                          | Some t -> ["t", string_of_int t] in
    let path = "/containers/" ^ id ^ "/restart" in
    unit_response_of_post "Docker.Container.restart" addr path q None ~id

  let rm ?(addr= !default_addr) ?(volumes=false) ?(force=false) ?(link=false)
        id =
    let q = ["v", string_of_bool volumes;
             "force", string_of_bool force;
             "link", string_of_bool link] in
    let path = "/containers/" ^ id in
    let status, _, body =
      response_of_delete "Docker.Container.rm" addr path q in
    if status >= 409 then
      raise(Failure("Docker.Container.rm", message_of_body body))
    else if status >= 404 then raise(No_such_container id)
    else if status >= 400 then
      (* Errors like "removal of container ... is already in progress"
         are reported with 400 — not a bad parameter problem! *)
      raise(Failure("Docker.Container.rm", message_of_body body))

  let kill ?(addr= !default_addr) ?signal id =
    let q = match signal with Some s -> ["signal", string_of_int s]
                            | None -> [] in
    let path = "/containers/" ^ id ^ "/kill" in
    unit_response_of_post "Docker.Container.kill" addr path q None ~id

  let pause ?(addr= !default_addr) id =
    let path = "/containers/" ^ id ^ "/pause" in
    unit_response_of_post "Docker.Container.pause" addr path [] None ~id

  let unpause ?(addr= !default_addr) id =
    let path = "/containers/" ^ id ^ "/unpause" in
    unit_response_of_post "Docker.Container.unpause" addr path [] None ~id


  let attach ?(addr= !default_addr)
             ?(stdin=false) ?(stdout=false) ?(stderr=false) id which =
    let logs, stream = match which with
      | `Logs ->   "true",  "false"
      | `Stream -> "false", "true"
      | `Logs_and_stream -> "true", "true" in
    let q = ["logs", logs;
             "stream", stream;
             "stdin", string_of_bool stdin;
             "stdout", string_of_bool stdout;
             "stderr", string_of_bool stderr ] in
    let path = "/containers/" ^ id ^ "/attach" in
    let fd = post "Docker.Containers.attach" addr path q None in
    let buf = Buffer.create 4096 in
    let status, _h = read_headers "Docker.Containers.attach" buf fd in
    deal_with_status_500 "Docker.Containers.attach" status fd;
    if status >= 400 then (
      Unix.close fd;
      if status >= 404 then raise(No_such_container id)
      else raise(Invalid_argument "Docker.Containers.attach")
    );
    (* FIXME: need to know whether the TTY setting is enabled by
       [create] — [false] at the moment. *)
    Stream.create buf fd

  module Exec = struct
    type t = string (* exec ID *)

    let create ?(addr= !default_addr) ?(stdin=false) ?(stdout=true)
          ?(stderr=true) ?detach_keys ?(env=[]) ?(privileged=false) ?user
          container cmd =
      let json =
        ["AttachStdin", `Bool stdin;
                "AttachStdout", `Bool stdout;
                "AttachStderr", `Bool stderr;
                "Tty", `Bool false;
                "Env", json_of_strings env;
                "Cmd", json_of_strings cmd;
                "Privileged", `Bool privileged ] in
      let json = match detach_keys with
        | Some d -> ("DetachKeys", `String d) :: json
        | None -> json in
      let json = match user with
        | Some u -> ("User", `String u) :: json
        | None -> json in
      let path = "/containers/" ^ container ^ "/exec" in
      let status, _, body = response_of_post "Docker.Container.Exec.create"
                              addr path [] (Some (`Assoc json)) in
      if status >= 400 then (
        (* Try to extract the container ID. *)
        try
          let m = message_of_body body in
          let i = String.index m ':' in
          let id = String.sub m (i + 2) (String.length m - i - 2) in
          raise(No_such_container id)
        with _ -> raise(No_such_container "unknown ID")
      );
      (* Extract ID *)
      match Json.from_string body with
      | `Assoc l ->
         (try string_of_json "Docker.Containers.Exec.create" (List.assoc "Id" l)
          with _ -> raise(Error("Docker.Containers.Exec.create",
                                "No ID returned")))
      | _ ->
         raise(Error("Docker.Container.Exec.create",
                     "Response must be an association list: " ^ body ))


    let start ?(addr= !default_addr) exec_id =
      let json = `Assoc ["Detach", `Bool false;
                         "Tty", `Bool false ] in
      let path = "/exec/" ^ exec_id ^ "/start" in
      let fd = post "Docker.Containers.Exec.start" addr path [] (Some json) in
      let buf = Buffer.create 4096 in
      let status, _h = read_headers "Docker.Containers.Exec.start" buf fd in
      deal_with_status_500 "Docker.Containers.Exec.start" status fd;
      if status >= 409 then (
        Unix.close fd;
        raise(Failure("Docker.Container.Exec.start",
                      "Container is stopped or paused"));
      )
      else if status >= 400 then (
        Unix.close fd;
        raise(Failure("Docker.Container.Exec.start", "No such exec instance"));
      );
      Stream.create buf fd


    (* TODO: Exec Resize *)
    ;;
  end
end

module Image = struct
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
    | _ -> raise(Error("Docker.Images.list",
                       "Invalid image: " ^ Json.to_string img))

  let list ?(addr= !default_addr) ?(all=false) () =
    let q = ["all", string_of_bool all] in
    let _, _, body = response_of_get "Docker.Images.list" addr
                                     "/images/json" q in
    match Json.from_string body with
    | `List l -> List.map image_of_json l
    | _ ->
       raise(Error("Docker.Images.list",
                   "Response must be a JSON list: " ^ body))

  type source =
    | Image of { name: string;  repo: string;  tag: string }
    | Src of string
    | Stdin of { len: int;  write : Unix.file_descr -> int -> unit }

  let create ?(addr= !default_addr) ?(platform="") from =
    let q = ["platform", platform] in
    let status = match from with
      | Image im ->
         let q = ("fromImage", im.name) :: ("repo", im.repo)
                 :: ("tag", im.tag) :: q in
         let status, _, _ = response_of_post "Docker.Image.create"
                              addr "/images/create" q None in
         status
      | Src url ->
         if url = "-" then
           raise(Invalid_argument("Docker.Image.create: Invalid URL '-'"));
         let q = ("fromSrc", url) :: q in
         let status, _, _ = response_of_post "Docker.Image.create"
                              addr "/images/create" q None in
         status
      | Stdin img ->
         (* We do not use the [response_of_post] function because we
            use a specialized version to insert the body. *)
         let buf = post_header "/images/create" q in
         Buffer.add_string buf "Content-Type: application/octet-stream\r\n\
                                Content-Length: ";
         Buffer.add_string buf (string_of_int img.len);
         Buffer.add_string buf "\r\n\r\n";
         let fd = send_buffer "Docker.Image.create" addr buf in
         img.write fd img.len;
         Unix.shutdown fd Unix.SHUTDOWN_SEND;
         let status, _, _ = read_response "Docker.Image.create" fd in
         Unix.close fd;
         status in
    if status >= 404 then
      raise(Failure("Docker.Image.create", "repository does not exist \
                                            or no read access"))

  let from_image ?(repo = "") ?(tag = "") name =
    Image { name;  repo;  tag }
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
  | _ -> raise(Error("Docker.version",
                     "Response must be a JSON association list: " ^ body))



;;
(* Local Variables: *)
(* compile-command: "make -k -w -C.." *)
(* End: *)
