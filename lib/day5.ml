open Core
open Angstrom
open OUnit
open Utils

type range = Range of int * int * int [@@deriving show]
type input = Input of int list * range list list [@@deriving show]
   
let rangeMapParser = 
  sep_by1 (char '\n') 
   ((fun a b c -> Range (a,b,c))
       <$> number <*> (char ' ' *> number) <*> (char ' ' *> number))
 
let parser = (fun a b -> Input (a,b)) <$> 
    (string "seeds: " *> sep_by (char ' ') number) <*>
    (string "\n\n" *> many1 (not_char '\n') *>
       char '\n' *> sep_by (string "\n\n" *> many1 (not_char '\n') *>
       char '\n') rangeMapParser)

let inRange loc (Range (_,s,r)) = loc >= s && loc < s+r

let lookup (item: int) (map: range list): int
  = List.fold_right 
        ~f:(fun (Range (d,s,r)) lookup -> if inRange item (Range (d,s,r)) 
                                          then item - s + d else lookup) map ~init:item 

let lookupSeed maps seed = List.fold_left ~f:lookup ~init:seed maps

let partA (Input (seeds,maps) : input): string = 
  Iter.of_list seeds |>
  Iter.map (lookupSeed maps) |>
  Iter.min_exn |> string_of_int

let partB (_ : input): string = "not yet implemented"

let tests = "tests" >::: [golden_test "day5" parser partA "35" ~printer:show_input;]
