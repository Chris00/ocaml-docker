
(** Interface to Docker Remote API.

   See the {{:https://docs.docker.com/engine/api/v1.29/}Docker API} if
   needed to complement the documentation below.

   @version %%VERSION%% *)


val set_default_addr : Unix.sockaddr -> unit
(** Set the address the Docker daemon listens to.  This will be used
    by all funtions of this API unless the optional parameter [~addr]
    is used. *)

exception No_such_container of string [@warn_on_literal_pattern]
(** [No_such_container id] is raised to notify that the container [id]
   does not exist. *)

exception Failure of string * string [@warn_on_literal_pattern]
(** [Failure(fn_name, msg)] is raised when the requested action fails
   to be performed (because, say, the container stopped,...).
   [fn_name] is the name of the function raising the error and [msg]
   an explanation. *)

exception Invalid_argument of string [@warn_on_literal_pattern]
(** [Invalid_argument msg] can be raised by any function
    when an incorrect argument.  [msg] is the function raising
    the exception. *)

exception Server_error of string [@warn_on_literal_pattern]
(** [Server_error fn_name] indicates that the server encountered
    an error or that the returned response is incorrect.  [fn_name] is
    the function raising the error. *)

exception Error of string * string [@warn_on_literal_pattern]
(** [Error(fn_name, msg)] is raised for connection of protocol errors.
    [fn_name] is the function raising the error and [msg] is a
    possible explanation for the error.  Typically, this exception
    should not be raised with a well behaving server. *)

(** A stream returned by some Docker functions. *)
module Stream : sig
  type t

  type kind = Stdout | Stderr

  exception Timeout

  val out : t -> out_channel
  (** [out stream] may be used to send data to the process running in
      the container.  Closing this channel is equivalent to calling
      {!close}. *)

  val shutdown : t -> unit
  (** [shutdown stream] transmit an end-of-file condition to the
      server reading on the other side of the connection meaning that
      you have finished sending data.  You can still read data from
      the string.  You must still close the string with {!close}. *)

  val read : ?timeout: float -> t -> kind * string
  (** [read stream] reads the next payload from the stream.  The byte
    sequence will be empty if there is nothing to read at the time of
    the call (in particular, if everything has been read).

    @raise Timeout if the payload could not be read within the allowed
    timeout.  A negative timeout (the default) means unbounded wait. *)

  val read_all : t ->  (kind * string) list
  (** Read all the available data on the stream. *)

  val close : t -> unit
  (** Close the stream. *)
end

module Container : sig
  type port = {
      priv: int;   (** Private port number. *)
      pub: int;    (** Public port number. *)
      typ: string; (** Type, e.g., "tcp". *)
    }

  type id = string

  type id_or_name = string

  type t = {
      id: id;    (** Identifier of the container. *)
      names: string list; (** Names given to the container. *)
      image: string; (** Name of the image used to create the container. *)
      command: string; (** Command passed to the container. *)
      created: float;  (** Unix time of creation. *)
      status: string;  (** Human readable status. *)
      ports: port list;
      size_rw: int option;
      size_root_fs: int option;
    }

  type bind =
    | Vol of string (** create a new volume for the container *)
    | Mount of string * string
    (** [Mount(host_path, container_path)] bind-mount a host path
        into the container.  A relative [host_path] will be interpreted
        as relative to the current working directory (at the time of
        the function calling this binding).  [container_path] must be an
        {i absolute} path inside the container. *)
    | Mount_ro of string * string
    (** As [Mount] but make the bind-mount read-only inside the container. *)

  val list : ?addr: Unix.sockaddr ->
             ?all: bool -> ?limit: int -> ?size: bool ->
             ?before: id_or_name ->
             ?exited: int list ->
             ?health: [`Starting | `Healthy | `Unhealthy | `None] list ->
             ?name: string list ->
             ?since: id_or_name ->
             ?status: [`Created | `Restarting | `Running | `Removing |
                       `Paused | `Exited | `Dead] list ->
             ?volume: string ->
             unit -> t list
  (** [list ()] lists running containers (or all containers if [~all]
      is set to [true]).

      @param all Show all containers. Only running containers are
                 shown by default (i.e., this defaults to [false]).
      @param limit Return this number of most recently created
                   containers, including non-running ones.
      @param size Return the size of container as fields [size_rw]
                  and [size_root_fs].

      The following options set filters on the returned container
      list: [before], [exited] (containers with exit code given by
      [exited]), [health], [name], [since], [status], [volume].
   *)

  type host_config = {
      cpu_shares : int;
      (** Represents this container's relative CPU weight versus other
         containers.  Non-positive values are ignored. *)
      memory : int;
      (** Memory limit in bytes. *)
      cgroup_parent : string;
      (** Path to cgroups under which the container's cgroup is
         created. If the path is not absolute, the path is considered
         to be relative to the cgroups path of the init
         process. Cgroups are created if they do not already exist. *)
      blk_io_weight : int;
      (** Block IO weight (relative weight).  Values outside
         [0 .. 1000] do not set this field. *)
      (* blk_io_weight_device : throttle_device; *)
      (* Block IO weight (relative device weight) in the form
         [{"Path": "device_path", "Weight": weight}]. *)
      (* blk_io_device_read_bps : throttle_device; *)
      (* Limit read rate (bytes per second) from a device, in the form
         [{"Path": "device_path", "Rate": rate}]. *)
      (* blk_io_device_write_bps : throttle_device; *)
      (* Limit write rate (bytes per second) to a device, in the form
         [{"Path": "device_path", "Rate": rate}]. *)
      (* blk_io_device_read_iops : throttle_device; *)
      (* Limit read rate (IO per second) from a device, in the form
         [{"Path": "device_path", "Rate": rate}]. *)
      (* blk_io_device_write_iops : throttle_device; *)
      (* Limit write rate (IO per second) to a device, in the form
         [{"Path": "device_path", "Rate": rate}]. *)
      cpu_period : int;
      (** The length of a CPU period in microseconds. Non-positive
         values do not set this field. *)
      (* cpu_quota : int64; *)
      (* Microseconds of CPU time that the container can get in a CPU period. *)
      (* cpu_realtime_period : int64; *)
      (* The length of a CPU real-time period in microseconds. Set to
         0 to allocate no time allocated to real-time tasks. *)
      (* cpu_realtime_runtime : int64; *)
      (* The length of a CPU real-time runtime in microseconds. Set to
         0 to allocate no time allocated to real-time tasks. *)
      (* cpuset_cpus : string; *)
      (* CPUs in which to allow execution (e.g., 0-3, 0,1) *)
      (* cpuset_mems : string; *)
      (* Memory nodes (MEMs) in which to allow execution (0-3,
         0,1). Only effective on NUMA systems. *)
      (* devices : device_mapping; *)
      (* A list of devices to add to the container. *)
      (* device_cgroup_rules : string; *)
      (* A list of cgroup rules to apply to the container *)
      (* disk_quota : int64; *)
      (* Disk limit (in bytes). *)
      (* kernel_memory : int; *)
      (* Kernel memory limit in bytes. *)
      (* memory_reservation : int; *)
      (* Memory soft limit in bytes. *)
      memory_swap : int;
      (** Total memory limit (memory + swap).  Set as -1 to enable
          unlimited swap. *)
      (* memory_swappiness : int; *)
      (* Tune a container's memory swappiness behavior. Accepts an
         integer between 0 and 100. *)
      (* nano_cpus : int; *)
      (* CPU quota in units of 10-9 CPUs. *)
      (* oom_kill_disable : bool; *)
      (* Disable OOM Killer for the container. *)
      (* pids_limit : int; *)
      (* Tune a container's pids limit. Set -1 for unlimited. *)
      (* ulimits : ulimits; *)
      (* A list of resource limits to set in the container. For
         example: {"Name": "nofile", "Soft": 1024, "Hard": 2048} *)
      (* cpu_count : int *)
      (* The number of usable CPUs (Windows only).

         On Windows Server containers, the processor resource controls
         are mutually exclusive. The order of precedence is CPUCount
         first, then CPUShares, and CPUPercent last. *)
      (* cpu_percent : int; *)
      (* The usable percentage of the available CPUs (Windows only).

         On Windows Server containers, the processor resource controls
         are mutually exclusive. The order of precedence is CPUCount
         first, then CPUShares, and CPUPercent last. *)
      (* io_maximum_iops : int; *)
      (* Maximum IOps for the container system drive (Windows only) *)
      (* io_maximum_bandwidth : int; *)
      (* Maximum IO in bytes per second for the container system drive
         (Windows only). *)
      binds : bind list;
      (** A list of volume bindings for this container. *)
      (* container_id_file : string; *)
      (* Path to a file where the container ID is written *)
      (* log_config : log_config; *)
      (* The logging configuration for this container *)
      network_mode : string;
      (** Network mode to use for this container. Supported standard
         values are: bridge, host, none, and container:<name|id>. Any
         other value is taken as a custom network's name to which this
         container should connect to. *)
      (* port_bindings : port_bindings; *)
      (* A map of exposed container ports and the host port they
         should map to. *)
      policy : [ `None | `Auto_remove | `Restart_always
               | `Restart_unless_stopped | `Restart_on_failure of int];
      (** The behavior to apply when the container exits.  The default
         is not to restart and not to remove the container ([`None]).
         An ever increasing delay (double the previous delay, starting
         at 100ms) is added before each restart to prevent flooding
         the server.

         - [`Auto_remove] Automatically remove the container when the
           container's process exits.
         - [`Restart_always] Always restart.
         - [`Restart_unless_stopped] Restart always except when the
           user has manually stopped the container.
         - [`Restart_on_failure n] Restart only when the container
           exit code is non-zero.  The number [n] says how many times
           to retry before giving up. *)
      (* volume_driver : string; *)
      (* Driver that this container uses to mount volumes. *)
      (* volumes_from : string; *)
      (* A list of volumes to inherit from another container,
         specified in the form <container name>[:<ro|rw>]. *)
      (* mounts : mount; *)
      (* Specification for mounts to be added to the container. *)
      (* cap_add : string; *)
      (* A list of kernel capabilities to add to the container. *)
      (* cap_drop : string; *)
      (* A list of kernel capabilities to drop from the container. *)
      (* dns : string; *)
      (* A list of DNS servers for the container to use. *)
      (* dns_options : string; *)
      (* A list of DNS options. *)
      (* dns_search : string; *)
      (* A list of DNS search domains. *)
      (* extra_hosts : string; *)
      (* A list of hostnames/IP mappings to add to the container's
         /etc/hosts file. Specified in the form ["hostname:IP"]. *)
      (* group_add : string; *)
      (* A list of additional groups that the container process will run as. *)
      (* ipc_mode : string; *)
      (* IPC namespace to use for the container. *)
      (* cgroup : string; *)
      (* Cgroup to use for the container. *)
      (* links : string; *)
      (* A list of links for the container in the form container_name:alias. *)
      (* oom_score_adj : int; *)
      (* An integer value containing the score given to the container
         in order to tune OOM killer preferences. *)
      (* pid_mode : string; *)
      (* Set the PID (Process) Namespace mode for the container. It
         can be either:

         "container:<name|id>": joins another container's PID namespace
         "host": use the host's PID namespace inside the container *)
      (* privileged : bool; *)
      (* Gives the container full access to the host. *)
      (* publish_all_ports : bool; *)
      (* Allocates a random host port for all of a container's exposed ports. *)
      (* readonly_rootfs : bool; *)
      (* Mount the container's root filesystem as read only. *)
      (* security_opt : string; *)
      (* A list of string values to customize labels for MLS systems,
         such as SELinux. *)
      (* storage_opt : storage_opt; *)
      (* Storage driver options for this container, in the form
         {"size": "120G"}. *)
      (* tmpfs : tmpfs; *)
      (* A map of container directories which should be replaced by
         tmpfs mounts, and their corresponding mount options. For
         example: { "/run": "rw,noexec,nosuid,size=65536k" }. *)
      (* uts_mode : string; *)
      (* UTS namespace to use for the container. *)
      (* userns_mode : string; *)
      (* Sets the usernamespace mode for the container when
         usernamespace remapping option is enabled. *)
      (* shm_size : int; *)
      (* integer >= 0
         Size of /dev/shm in bytes. If omitted, the system uses 64MB. *)
      (* sysctls : sysctls; *)
      (* A list of kernel parameters (sysctls) to set in the
         container. For example: {"net.ipv4.ip_forward": "1"} *)
      (* runtime : string; *)
      (* Runtime to use with this container. *)
      (* console_size : int; *)
      (* integer >= 0
         Initial console size, as an [height, width] array. (Windows only) *)
      (* isolation : string; *)
      (* "default" "process" "hyperv"
         Isolation technology of the container. (Windows only) *)
    }

  val host :
    ?cpu_shares: int ->
    ?memory: int ->
    ?cgroup_parent: string ->
    ?blk_io_weight: int ->
    ?cpu_period: int ->
    ?memory_swap: int ->
    ?binds: bind list ->
    ?network_mode: string ->
    ?policy: [ `Auto_remove | `None | `Restart_always
             | `Restart_on_failure of int | `Restart_unless_stopped ] ->
    unit -> host_config
  (** Return the default host configuration changed according to which
     optional labels were set. *)

  val create :
    ?addr: Unix.sockaddr ->
    ?hostname: string -> ?domainname: string -> ?user: string ->
    ?stdin: bool -> ?stdout: bool -> ?stderr: bool ->
    ?open_stdin: bool -> ?stdin_once: bool ->
    ?env: string list -> ?workingdir: string -> ?networking: bool ->
    ?host: host_config ->
    ?name: string ->
    string -> string list -> id
  (** [create image cmd] create a container and returns its ID where
    [image] is the image name to use for the container and [cmd] the
    command to run.  [cmd] has the form [[prog; arg1;...; argN]].
    BEWARE that the output of [cmd] (on stdout and stderr) will be
    logged by the container (see {!logs}) so it will consume disk space.

    @param hostname the desired hostname to use for the container.
    @param domainname  the desired domain name to use for the container.
    @param user the user (or UID) to use inside the container.
    @param stdin Attaches to stdin (default [false]).
    @param stdout Attaches to stdout (default [true]).
    @param stdout Attaches to stderr (default [true]).
    @param open_stdin opens stdin (sic).
    @param stdin_once Close stdin after the 1 attached client disconnects.
                      Default: [false].
    @param env A list of environment variables of the form ["VAR=value"].
               A variable without = is removed from the environment, rather
               than to have an empty value.
    @param workingdir The working dir for commands to run in.
    @param networking Whether networking is enabled for the container.
                      Default: [false].
    @param name The name of the container.  The name must match
       [/?[a-zA-Z0-9_-]+] or [Invalid_argument] is raised.  It can be
       used in place of the container ID.  If the name exists (whether
       the container is running or not), the container will not be
       recreated.  Note that the corresponding name in {!t} (as
       obtained by {!list}) will have an initial '/' which means that
       the Docker daemon is the parent container. *)

  (* val inspect : ?addr: Unix.sockaddr -> id -> t *)

  (* val top : conn -> id -> *)

  (* val logs : conn -> id -> *)

  val changes : ?addr: Unix.sockaddr ->
                id -> (string * [`Modified | `Added | `Deleted]) list
  (** [changes conn id] returns which files in a container's
     filesystem have been added, deleted, or modified. *)

  (* val export : ?addr: Unix.sockaddr -> id -> stream *)
  (** [export conn id] export the contents of container [id]. *)

  val start : ?addr: Unix.sockaddr -> ?detach_keys: string ->
              id -> unit
  (** [start id] starts the container [id].

    @raise Server_error when, for example, if the command given by
     {!create} does not exist in the container.

    @param detach_keys override the key sequence for detaching a
     container. Format is a single character [[a-Z]] or [ctrl-<value>]
     where <value> is one of: [a-z], [@], [^], [\[], [,] or [_]. *)

  val stop : ?addr: Unix.sockaddr -> ?wait: int -> id -> unit
  (** [stop id] stops the container [id].
      @param wait number of seconds to wait before killing the container. *)

  val wait : ?addr: Unix.sockaddr -> id -> int
  (** [wait id] block until a container [id] stops, then returns the
     exit code.  *)

  val restart : ?addr: Unix.sockaddr -> ?wait: int -> id -> unit
  (** [restart id] restart the container [id].

    @param wait number of seconds to wait before killing the container. *)

  val kill : ?addr: Unix.sockaddr -> ?signal: int -> id -> unit
  (** [kill id] kill the container [id].

    @param signal Signal to send to the container (see the standard
    module [Sys]).  When not set, [Sys.sigkill] is assumed and the
    call will waits for the container to exit.  *)

  val pause : ?addr: Unix.sockaddr -> id -> unit
  (** [pause id] pause the container [id]. *)

  val unpause : ?addr: Unix.sockaddr -> id -> unit
  (** [unpause id] unpause the container [id]. *)


  val attach : ?addr: Unix.sockaddr ->
               ?stdin: bool -> ?stdout: bool -> ?stderr: bool ->
               id -> [`Logs | `Stream | `Logs_and_stream] -> Stream.t
  (** [attach id what] view or interact with any running container
     [id] primary process (pid 1).
     - If [what = `Logs] replay the logs from the container: you will
       get the output since the container started.
     - If [what = `Stream], stream [stdin], [stdout] and [stderr] (if
       enabled) from the time the request was made onwards.
     - If [what = `Logs_and_stream] after getting the output of
       [`Logs], it will seamlessly transition into streaming current
       output.

     @param stdin attach to stdin.  Default [false].
     @param stdout return and/or attach to stdout.  Default [false].
     @param stderr return and/or attach to stderr.  Default [false]. *)

  val rm : ?addr: Unix.sockaddr -> ?volumes: bool -> ?force: bool ->
           ?link: bool ->
           id -> unit
  (** [rm id] remove the container [id] from the filesystem.
    @raise Docker.Invalid_argument if the container does not exist.

    @param volumes Remove the volumes associated to the container.
                   Default [false].
    @param force Kill then remove the container.  Default [false].
    @param link Remove the specified link associated with the container.
                Default: [false].  *)

  module Exec : sig
    type t

    val create : ?addr: Unix.sockaddr ->
                 ?stdin: bool -> ?stdout: bool -> ?stderr: bool ->
                 id -> string list -> t
    (** [exec id cmd] sets up an exec instance in the {i running}
      container [id] that executes [cmd].  [cmd] has the form [[prog;
      arg1;...; argN]].  This command will not be restarted if the
      container is restarted.  If the container is paused, then the
      command will wait until the container is unpaused, and then run.
      The output of this command is not logged by the container.  If
      the command does not exist, an message will be printed on the
      stderr component of the stream returned by {!start}.

      @param stdin whether to attach stdin.  Default: [false].
      @param stdout whether to attach stdout.  Default: [true].
      @param stderr whether to attach stderr.  Default: [true]. *)

    val start : ?addr: Unix.sockaddr -> t -> Stream.t
    (** [start exec_id] starts a previously set up exec instance
      [exec_id].  Returns a stream that enable an interactive session
      with the command. *)
  end
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

type version = { api_version: string;
                 version: string;
                 git_commit: string;
                 go_version: string }

val version : ?addr: Unix.sockaddr -> unit -> version

(* val ping : ?addr: Unix.sockaddr -> unit -> [`Ok | `Error] *)

(* val commit : ?addr: Unix.sockaddr -> unit -> Image.t *)

(* val monitor : ?addr: Unix.sockaddr -> unit -> event list *)

(* val get : ?addr: Unix.sockaddr -> name: string -> stream *)

(* val load : ?addr: Unix.sockaddr -> tarball: string -> unit *)
(* FIXME: More general than tarball in memory *)


;;
