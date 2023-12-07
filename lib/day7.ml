open Core
open Angstrom
open OUnit
open Utils

type input = (char list * int) list [@@deriving show]

let hand = (fun a b -> (a,b)) <$> many1 (not_char ' ') <*> (char ' ' *> number)

let parser = sep_by1 (char '\n') hand

let composeCompare c1 c2 a b = let r1 = c1 a b in if r1 = 0 then c2 a b else r1

let mapCompare c1 ~f a b = c1 (f a) (f b)

let value c = 
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | _ -> Char.get_digit_exn c

let fallback = List.compare (mapCompare compare ~f:value)

let handvalue (a: char list) = 
  let lengths = List.sort ~compare:Char.compare a |> List.group ~break:Char.(<>) |> 
                List.map ~f:List.length |> List.sort ~compare:compare in
  match lengths with
  | [5] -> 6
  | [1;4] -> 5
  | [2;3] -> 4
  | [1;1;3] -> 3
  | [1;2;2] -> 2
  | [_;_;_;2] -> 1
  | [1;1;1;1;1] -> 0
  | _ -> raise (UtilException "error")

let main_case = mapCompare compare ~f:handvalue

let partAcompare = composeCompare main_case fallback

let partA (i : input): string = 
    List.sort ~compare:(mapCompare ~f:(fun (a,b) -> a) partAcompare) i |>
    List.mapi ~f:(fun i (_,a) -> (i+1) * a) |>
    List.fold ~init:0 ~f:(+) |> string_of_int


let valueJ c = 
  match c with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 1
  | 'T' -> 10
  | _ -> Char.get_digit_exn c

let handvalueB (a: char list) = 
  let num_jokers = List.filter ~f:(fun a -> Char.(a = 'J')) a |> List.length in
  let lengths = List.sort ~compare:Char.compare a |> List.group ~break:Char.(<>) |> 
                List.map ~f:List.length |> List.sort ~compare:compare in
  match lengths with
  | [5] -> 6
  | [1;4] -> if num_jokers = 0 then 5 else 6
  | [2;3] -> if num_jokers = 0 then 4 else 6
  | [1;1;3] -> if num_jokers = 0 then 3 else 5
  | [1;2;2] -> if num_jokers = 0 then 2 else if num_jokers = 2 then 5 else 4
  | [_;_;_;2] -> if num_jokers = 0 then 1 else 3
  | [1;1;1;1;1] -> if num_jokers = 0 then 0 else 1
  | _ -> raise (UtilException "error")

let main_caseB = mapCompare compare ~f:handvalueB

let fallback = List.compare (mapCompare compare ~f:valueJ)

let partBcompare = composeCompare main_caseB fallback

let partB (i : input): string = 
    List.sort ~compare:(mapCompare ~f:(fun (a,b) -> a) partBcompare) i |>
    List.mapi ~f:(fun i (_,a) -> (i+1) * a) |>
    List.fold ~init:0 ~f:(+) |> string_of_int

let tests = "tests" >::: [golden_test "day7" parser partA "6440" ~printer:show_input;
                          golden_test "day7" parser partB "5905" ~printer:show_input; ]
