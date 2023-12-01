open Core
open Cmdliner

module type day = sig
  type input
  val show_input : input -> string
  val parser : input Angstrom.t
  val partA : input -> string
  val partB : input -> string
  val tests: OUnit.test
end

module Day (V : day)  = struct
    let runDay daynum verbose = 
       let _ = OUnit.run_test_tt V.tests in
       let contents = In_channel.read_all ("inputs/day" ^ string_of_int daynum) in
       let data = Angstrom.parse_string ~consume:Angstrom.Consume.Prefix  V.parser contents in
       match data with
        | Error e -> print_endline e
        | Ok a -> if verbose then print_endline (V.show_input a) else ();
                  print_endline "======= Part A =======";
                  print_endline (V.partA a);
                  print_endline "======= Part B =======";
                  print_endline (V.partB a)
end

module D1 = Day (Days.Day1)

let days = [D1.runDay]

let run day verbose = 
  if day > 25 || day < 1 then print_endline "invalid day" 
  else List.nth_exn days (day - 1) day verbose 

let day = 
  let doc = "execute day number $(docv)" in
  Arg.(value & opt int 0 & info ["d";"day"] ~docv:"DAY_NUMBER" ~doc)

let verbose = 
  let doc = "print the parser output" in
  Arg.(value & flag & info ["v";"verbose"] ~docv:"VERBOSE" ~doc)

let cmd = 
  let doc = "advent of code solutions" in
  let man = [
        `S Manpage.s_description;] in
  let info = Cmd.info "aoc2023" ~version:"0.0" ~doc ~man in
  Cmd.v info Term.(const run $ day $ verbose)
    

let () = exit (Cmd.eval cmd)
