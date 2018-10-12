open Printf

let () =
  let v = try Docker.version()
          with Docker.Error(_, msg) ->
            printf "version: Error: %s\n" msg;
            exit 0 in
  let open Docker in
  printf "API version: %s\n" v.api_version;
  printf "Version:     %s\n" v.version;
  printf "Git commit:  %s\n" v.git_commit;
  printf "Go version:  %s\n" v.go_version
