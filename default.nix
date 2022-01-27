{ concurrently, dockerTools, writeShellScriptBin, simpleSurvey }:

{
  # Docker image
  image = dockerTools.buildLayeredImage {
    name = "simpleSurvey";
    tag = "latest";
    contents = with simpleSurvey; [ server.server frontend.static ];
    config = {
      Cmd = [ "simpleSurvey-server" ];
      ExposedPorts = { "8000/tcp" = { }; };
    };
  };

  # Launch development server
  dev = writeShellScriptBin "dev" ''
    rm -rf ./node_modules
    ln -s ${simpleSurvey.frontend.nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${simpleSurvey.frontend.nodeDependencies}/bin:$PATH"
    nix develop --command ${concurrently}/bin/concurrently \
      -n FE,BE \
      -c green,red \
      "cd frontend && npm start" \
      "cd server && hpack && cabal build && ghcid -r app/Main"
  '';
}
