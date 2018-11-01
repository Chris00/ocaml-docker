
let () =
  Common.install_image "alpine" ~tag:"latest";
  let c = Docker.Container.create "alpine:latest" ["echo"; "Hello"; "World"] in
  Docker.Container.start c;
  let _code = Docker.Container.wait c in
  Docker.Container.stop c;
  Docker.Container.rm c
