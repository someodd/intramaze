{ mkDerivation, aeson, base, bytestring, containers, cryptonite
, data-default, dir-traverse, directory, either, exceptions
, filepath, frontmatter, heroku, http-api-data, http-types
, lib, libjwt-typed, memory, monad-logger, monad-time
, mtl, mustache, path-pieces, persistent, persistent-postgresql
, postgresql-simple, protolude, regex-compat, scotty, slug, text
, text-show, time, transformers, unordered-containers, utf8-string
, uuid, vector, wai, wai-cors, wai-extra, wai-websockets, warp
, websockets
}:
mkDerivation {
  pname = "Interwebz";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite data-default
    dir-traverse directory either exceptions filepath frontmatter
    heroku http-api-data http-types libjwt-typed memory
    monad-logger monad-time mtl mustache path-pieces persistent
    persistent-postgresql postgresql-simple protolude regex-compat
    scotty slug text text-show time transformers unordered-containers
    utf8-string uuid vector wai wai-cors wai-extra wai-websockets warp
    websockets
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
