(** Interface to Docker Remote API <https://www.docker.com>. *)


val set_default_addr : Unix.sockaddr -> unit
(** Set the address the Docker daemon listens to.  This will be used
    by all funtions of this API unless the optioal parameter [~addr]
    is used. *)

exception Invalid_argument of string
(** [Invalid_argument msg] can be raised by any function
    when an argument is incorrect.  [msg] is the function raising
    the exception possibly followed by an explanation. *)

exception Server_error of string
(** [Server_error msg] indicates that the server encountered an error
    or that the returned response is incorrect.  [msg] is the name of
    the function raising the exception (possibly with some
    message). *)


module Container : sig
  type port = {
      priv: int;   (** Private port number. *)
      pub: int;    (** Public port number. *)
      typ: string; (** Type, e.g., "tcp". *)
    }

  type id = string

  type t = {
      id: id;    (** Identifier of the container. *)
      names: string list; (** Names given to the container. *)
      image: string; (** Name of the image used to create the container. *)
      command: string; (** Command passed to the container. *)
      created: float;  (** Unix time of creation. *)
      status: string;  (** Human readable status. *)
      ports: port list;
      size_rw: int;
      size_root_fs: int;
    }

  val list : ?addr: Unix.sockaddr ->
             ?all: bool -> ?limit: int -> ?since: id -> ?before: id ->
             ?size: bool ->
             unit -> t list
  (** [list ()] lists running containers (or all containers if [~all]
      is set to [true]).

      @param all Show all containers. Only running containers are
                 shown by default (i.e., this defaults to [false]).
   *)

  val create :
    ?addr: Unix.sockaddr ->
    ?hostname: string -> ?domainname: string -> ?user: string ->
    ?memory: int -> ?memory_swap: int ->
    ?attach_stdin: bool -> ?attach_stdout: bool -> ?attach_stderr: bool ->
    ?tty: bool -> ?open_stdin: bool -> ?stdin_once: bool ->
    ?env: string list -> ?workingdir: string -> ?networking: bool ->
    ?binds: (string * string * [`RO|`RW]) list ->
    image: string -> string -> id
  (** [create image cmd] create a container and returns its ID
    where [image] is the image name to use for the container and
    [cmd] the command to run.

    @param hostname the desired hostname to use for the container.
    @param domainname  the desired domain name to use for the container.
    @param user the user (or UID) to use inside the container.
    @param memory Memory limit in bytes.
    @param memory_swap Total memory usage (memory + swap);
                       set -1 to disable swap.
    @param attach_stdin Attaches to stdin (default [false]).
    @param attach_stdout Attaches to stdout (default [true]).
    @param attach_stdout Attaches to stderr (default [true]).
    @param tty Attach standard streams to a tty, including stdin if it
               is not closed.  Default: [false].
    @param open_stdin opens stdin (sic).
    @param stdin_once close stdin after the 1 attached client disconnects.
    @param env A list of environment variables n the form of ["VAR=value"].
    @param workingdir The working dir for commands to run in.
    @param networking Whether networking is enabled for the container.
                      Default: [false].
    @param binds A list of volume bindings for this container. Each volume
                 binding has the form [(host_path, container_path, access)].
   *)

  (* val inspect : ?addr: Unix.sockaddr -> id -> t *)

  (* val top : conn -> id -> *)

  (* val logs : conn -> id -> *)

  (* val changes : conn -> id -> *)
  (** [changes conn id] Inspect changes on container [id]'s filesystem. *)

  (* val export : ?addr: Unix.sockaddr -> id -> stream *)
  (** [export conn id] export the contents of container [id]. *)

  val start : ?addr: Unix.sockaddr ->
              ?binds: (string * string * [`RO|`RW]) list ->
              id -> unit

  val stop : ?addr: Unix.sockaddr -> ?wait: int -> id -> unit
  (** [stop id] stops the container [id].
      @param wait number of seconds to wait before killing the container. *)

  (* val restart : ?addr: Unix.sockaddr -> ?time: float -> id -> unit *)

  (* val kill : ?addr: Unix.sockaddr -> ?signal: int -> id -> unit *)

  (* val attach : ?addr: Unix.sockaddr -> id -> stream *)

  (* val wait : ?addr: Unix.sockaddr -> id -> int *)

  val rm : ?addr: Unix.sockaddr -> ?volumes: bool -> ?force: bool ->
           id -> unit
  (** [rm id] remove the container [id] from the filesystem.
    @raise Docker.Invalid_argument if the container does not exist.

    @param volumes Remove the volumes associated to the container.
                     Default [false].
    @param force Kill then remove the container.  Default [false]. *)

  (* val copy_file : ?addr: Unix.sockaddr -> id -> string -> stream *)
  ;;
end

module Images : sig
  type id = string

  type t = {
      id: id;
      created: float;
      size: int;
      virtual_size: int;
      tags: string list;
    }

  val list : ?addr: Unix.sockaddr -> ?all: bool -> unit -> t list

  (* val create : ?addr: Unix.sockaddr -> *)
  (*              ?from_image: string -> ?from_src: string -> ?repo: string -> *)
  (*              ?tag: string -> ?registry: string *)
  (*              -> stream *)

  (* val insert : ?addr: Unix.sockaddr -> name -> string -> stream *)

  (* val inspect : ?addr: Unix.sockaddr -> name -> t *)

  (* type history = { id: string;  created: float;  created_by: string } *)

  (* val history : ?addr: Unix.sockaddr -> name -> history list *)

  (* val push : ?addr: Unix.sockaddr -> name -> stream *)

  (* val tag : ?addr: Unix.sockaddr -> ?repo: string -> ?force:bool -> name -> unit *)

  (* val rm : ?addr: Unix.sockaddr -> name -> stream *)

  (* type search_result = { description: string; *)
  (*                        is_official: bool; *)
  (*                        is_trusted: bool; *)
  (*                        name: name; *)
  (*                        star_count: int } *)

  (* val search : ?addr: Unix.sockaddr -> string -> search_result list *)

  (* val build : ?addr: Unix.sockaddr -> unit -> stream *)

end

(* val auth : ?addr: Unix.sockaddr -> *)
(*            ?username:string -> ?password: string -> ?email: string -> *)
(*            ?serveraddress: string -> unit *)
(* (\** Get the default username and email. *\) *)

(* type info *)

(* val info : ?addr: Unix.sockaddr -> unit -> info *)

(* type version = { version: string; *)
(*                  git_commit: string; *)
(*                  go_version: string } *)

(* val version : ?addr: Unix.sockaddr -> unit -> version *)

(* val ping : ?addr: Unix.sockaddr -> unit -> [`Ok | `Error] *)

(* val commit : ?addr: Unix.sockaddr -> unit -> Image.t *)

(* val monitor : ?addr: Unix.sockaddr -> unit -> event list *)

(* val get : ?addr: Unix.sockaddr -> name: string -> stream *)

(* val load : ?addr: Unix.sockaddr -> tarball: string -> unit *)
(* FIXME: More general than tarball in memory *)


;;
