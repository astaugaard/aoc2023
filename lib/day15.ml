open Angstrom
open Utils
open OUnit

type input = string list [@@deriving show]
    
let parser = sep_by (char ',') (take_while1 (fun c -> c <> ',' && c <> '\n'))


let hash s = Iter.of_str s |>
  Iter.fold (fun cvalue c -> ((Char.code c + cvalue)*17) mod 256) 0

let partA (i : input): string = 
  Iter.of_list i |>
  Iter.map (fun s -> hash s) |> Iter.sum |> string_of_int

open Core

type instr = Delete | Insert of int [@@deriving show]

type state = (string * int) list [@@deriving show]

let stepParser = (fun n o -> (n,o)) <$> take_while1 (Char.is_alpha) <*> (((fun _ -> Delete) <$> char '-') <|> ((fun n -> Insert n) <$> (char '=' *> number)))

let add list ~equal key value = 
  let rec addgo ls = 
    match ls with
    | ((s,v)::ls) -> if equal s key then ((key,value)::ls, true) else 
        (match addgo ls with
        | (a,b) -> ((s,v)::a,b))
    | [] -> ([],false) in
  match addgo list with
  | (ls,b) -> if b then ls else (key,value)::ls
        
          


let partB (i : input): string = 
  let ar = Array.create ~len:256 [] in (* arrays are from back to front *)
  List.iter ~f:(fun s -> 
    let parsed = parse_string ~consume:Consume.Prefix stepParser s in 
    (match parsed with
    | Error _ -> raise (UtilException "nooooo")
    | Ok (label,inst) -> 
      let loc = hash label in
      let current = Array.get ar loc in
      match inst with
      | Delete -> Array.set ar loc (List.filter current ~f:(fun (s,_) -> String.(s <> label)))
      | Insert i -> Array.set ar loc (add current ~equal:(fun a b  -> String.(a = b)) label i) );
    ) i;
  Array.mapi ar ~f:(fun bi l -> 
    List.rev l |> 
    Iter.of_list |>
    Iter.mapi (fun li (_,f) -> f * (li+1) * (bi+1)) |> Iter.sum) |> Iter.of_array |> Iter.sum |> string_of_int

let tests = "tests" >::: [golden_test "day15" parser partA "1320" ~printer:show_input; golden_test "day15" parser partB "145" ~printer:show_input;]
