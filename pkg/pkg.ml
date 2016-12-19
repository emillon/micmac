#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "micmac" @@ fun c ->
  Ok [
    Pkg.bin "bin/rogue";
  ]
