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
  (fun a b -> (to_string a,
    Map.of_alist_exn (module String) b)) <$> (many1 (char 'L' <|> char 'R')) <*> (string "\n\n" *> sep_by1 (char '\n') line)

let lookup_next m (current,num) route = 
  let loc = Map.find_exn m current in
  let (next:string) = match (route,loc) with
  | ('L',(l,_)) -> l
  | ('R',(_,r)) -> r
  | _ -> raise (UtilException "oops")
  in ((next,num+1),if String.(next = "ZZZ") then `Stop else `Continue)

let partA ((lrs,m) : input): string = 
  match Iter.cycle (Iter.of_str lrs) |>
        Iter.fold_while (lookup_next m) ("AAA",0) with
  | (_,n) -> string_of_int n
  
type loop_info = (int * int list)

let lookup_nextB endcond lastIdx m (current,path) (idx,route) = 
  let loc = Map.find_exn m current in
  let (next:string) = match (route,loc) with
  | ('L',(l,_)) -> l
  | ('R',(_,r)) -> r
  | _ -> raise (UtilException "oops")
  in ((next, next::path),if String.(next = endcond) && idx = lastIdx then `Stop else `Continue)

let get_loop_info m location lrs = 
  let (_,steps) = Iter.cycle (Iter.pair_with_idx (Iter.of_str lrs)) |> 
                  Iter.fold_while (lookup_nextB location (String.length lrs - 1) m) (location,[]) in
  let indexes = List.rev steps |> 
                Iter.of_list |> 
                Iter.mapi (fun a b -> (a,b)) |> 
                Iter.filter (fun (_,b) -> Char.(b.[2] = 'Z')) |> 
                Iter.map (fun (a,_) -> a) in
  (List.length steps, indexes |> Iter.to_list)

let rec gcd a b =
 if a = 1 || b = 1 then 1 else
 if a < b then let mb = b mod a in if mb = 0 then a else gcd a mb else
 if a = b then a else
 let ma = a mod b in if ma = 0 then b else gcd (a mod b) b

let lcm a b = a * b / gcd a b

let partB ((lrs,m) : input): string = 
  let a = Map.to_alist m |> Iter.of_list |> Iter.map (fun (a,_) -> a) |> Iter.filter (fun a -> Char.(a.[2] = 'Z')) in
  let cycles = Iter.map (fun a -> get_loop_info m a lrs) a in
  Iter.fold (fun a (b,_) -> lcm a b) 1 cycles |> string_of_int (* this will not work on all inputs I think I got lucky *)

let tests = "tests" >::: [golden_test "day8a" parser partA "2" ~printer:show_input;
                          golden_test "day8b" parser partA "6" ~printer:show_input;
                          golden_test "day8c" parser partB "6" ~printer:show_input;]
