
let ls () =
  Common.install_image "alpine" ~tag:"latest";
  let c = Docker.Container.create "alpine:latest" ["uname"; "-a"]
            ~name:"test" ~open_stdin:true in
  Docker.Container.start c;
  let _e = Docker.Container.Exec.create c ["ls"; "-l"; "/"] in

  Docker.Container.stop c;
  Docker.Container.rm c
