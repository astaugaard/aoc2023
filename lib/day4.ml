open Core
open Angstrom
open Utils
open OUnit

type card = Card of int * int list * int list [@@deriving show]

type input = card list [@@deriving show]

let seperator = many1 (char ' ')   
 
let cardParse = 
  (fun c w n -> Card (c,w,n)) <$> 
  (string "Card" *> seperator *> number <* char ':') <*> 
  (seperator *> sep_by seperator number) <*>
  (seperator *> char '|' *> seperator *> sep_by seperator number)
 
let parser = sep_by1 (char '\n') cardParse


let make_set l = Set.of_list (module Int) l


let numberOfMatches (Card (_,w,cs)):int = 
  let want = make_set w in
  let have = make_set cs in 
  Set.length (Set.inter want have)

let value c:int = 
  let numMatches = numberOfMatches c in
  if numMatches = 0 then 0 else Base.(2 ** (numMatches - 1))

let partA (cards : input): string = 
  Iter.of_list cards |>
  Iter.map value |>
  Iter.sum |> string_of_int

let numberOfCards card others = 
  let num = numberOfMatches card in
  let get = List.take others num in
  let total_from = List.fold get ~init:0 ~f:(+) in
  List.cons (1 + total_from) others
  

let partB (i : input): string = 
    let cvs = List.fold_right i ~init:[] ~f:numberOfCards 
    in List.fold cvs ~init:0 ~f:(+) |> string_of_int

let tests = "tests" >::: [golden_test "day4" parser partA "13" ~printer:show_input;
                          golden_test "day4" parser partB "30" ~printer:show_input]
