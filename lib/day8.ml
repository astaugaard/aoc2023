open Core
open Angstrom
open OUnit
open Utils

type leftRight = string * string [@@deriving show]

type input = string * (string,leftRight,String.comparator_witness) Map.t

type showableInput = string * (string * leftRight) list [@@deriving show]

let show_input (dirs,mapThingy) = let map = Map.to_alist mapThingy in
    show_showableInput (dirs,map)


let to_string a = Iter.of_list a |> Iter.to_str

let line = (fun a b c -> (to_string a,(to_string b,to_string c))) <$> many1 (satisfy (Char.is_alpha)) <*> (string " = (" *> many1 (satisfy (Char.is_alpha))) <*> (string ", " *> many1 (satisfy (Char.is_alpha)) <* string ")")
    
let parser = 
  (fun a b -> Map.(to_string a,
    Map.of_alist_exn (module String) b)) <$> (many1 (char 'L' <|> char 'R')) <*> (string "\n\n" *> sep_by1 (char '\n') line)

let lookup_next m (current,num) route = 
  let loc = Map.find_exn m current in
  let (next:string) = match (route,loc) with
  | ('L',(l,_)) -> l
  | ('R',(_,r)) -> r
  in ((next,num+1),if String.(next = "ZZZ") then `Stop else `Continue)

let partA ((lrs,m) : input): string = 
  match Iter.cycle (Iter.of_str lrs) |>
        Iter.fold_while (lookup_next m) ("AAA",0) with
  | (_,n) -> string_of_int n
  

let partB (_ : input): string = "not yet implemented"

let tests = "tests" >::: [golden_test "day8a" parser partA "2" ~printer:show_input;
                          golden_test "day8b" parser partA "6" ~printer:show_input; ]
