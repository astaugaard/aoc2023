(* open Core *)
open Angstrom
open OUnit

type input = () [@@deriving show]
    
let parser = map (char '\n') ~f:(fun _ -> ())

let partA (_ : input): string = "not yet implemented"

let partB (_ : input): string = "not yet implemented"

let tests = "tests" >::: [TestCase((fun _ -> ()))]
