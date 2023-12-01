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
                  let beforeTime = Time_ns.now () in 

                  print_endline (V.partA a);

                  let elapsed = Time_ns.diff (Time_ns.now ()) beforeTime  in
                  print_endline ("elapsed time:" ^ (Time_ns.Span.to_string elapsed));

                  print_endline "======= Part B =======";
                  let beforeTimeB = Time_ns.now () in 

                  print_endline (V.partB a);

                  let elapsedB = Time_ns.diff (Time_ns.now ()) beforeTimeB in
                  print_endline ("elapsed time:" ^ (Time_ns.Span.to_string elapsedB));
end

module D1 = Day (Days.Day1)
module D2 = Day (Days.Day2)
module D3 = Day (Days.Day3)
module D4 = Day (Days.Day4)
module D5 = Day (Days.Day5)
module D6 = Day (Days.Day6)
module D7 = Day (Days.Day7)
module D8 = Day (Days.Day8)
module D9 = Day (Days.Day9)
module D10 = Day (Days.Day10)
module D11 = Day (Days.Day11)
module D12 = Day (Days.Day12)
module D13 = Day (Days.Day13)
module D14 = Day (Days.Day14)
module D15 = Day (Days.Day15)
module D16 = Day (Days.Day16)
module D17 = Day (Days.Day17)
module D18 = Day (Days.Day18)
module D19 = Day (Days.Day19)
module D20 = Day (Days.Day20)
module D21 = Day (Days.Day21)
module D22 = Day (Days.Day22)
module D23 = Day (Days.Day23)
module D24 = Day (Days.Day24)
module D25 = Day (Days.Day25)

let days = [D1.runDay;
    D2.runDay;
    D3.runDay;
    D4.runDay;
    D5.runDay;
    D6.runDay;
    D7.runDay;
    D8.runDay;
    D9.runDay;
    D10.runDay;
    D11.runDay;
    D12.runDay;
    D13.runDay;
    D14.runDay;
    D15.runDay;
    D16.runDay;
    D17.runDay;
    D18.runDay;
    D19.runDay;
    D20.runDay;
    D21.runDay;
    D22.runDay;
    D23.runDay;
    D24.runDay;
    D25.runDay]

let run day verbose = 
  if day = 0 then List.iteri days 
                  ~f:(fun i f -> Printf.printf "======= Day %i =======" (i+1); 
                                 f (i+1) verbose) else
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
