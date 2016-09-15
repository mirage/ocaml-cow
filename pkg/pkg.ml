#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "cow" @@ fun c ->
  Ok [ Pkg.mllib "src/cow.mllib";
       Pkg.test "test/test"; ]
