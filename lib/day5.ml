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

let rec makeRanges l = match l with
  | a::b::bs -> (a,b)::makeRanges bs
  | _ -> []

type nrange = NRange of int * int

let sortRanges = List.sort ~compare:(fun (a,_) (b,_) -> compare a b)

let rec mapRanges map ranges = 
  match (map,ranges) with
  | (_,[]) -> []
  | ([],a) -> a
  | (((Range (d,s,a))::bs),((st,am)::rs)) ->
       if (st + am) < s then (st,am)::mapRanges map rs (* completely before *)
       else if (st+am) >= s && st < s then (st,s - st)::mapRanges map ((s,am + st - s)::rs) (* partialy before check if this is right*)
       else if st >= s && (st + am) <= (s + a) then (st + d - s,am)::mapRanges map rs  (* covered *)
       else if st >= s && st < (s + a) then (st + d - s,s+a - st)::mapRanges map ((s+a,st + am - s - a) :: rs)  (* partially in going out *) (* 45 19   64 6 *)
       else if st >= (s+a) then mapRanges bs ranges else raise (UtilException "thing") (* after thing *)

let lookupRanges maps seedRanges = List.fold_left 
    ~f:(fun acc m -> let sortedMap = List.sort m ~compare:(fun (Range (_,a,_)) (Range (_,b,_)) -> compare a b) in 
                     mapRanges sortedMap acc |> sortRanges)
                        
    ~init:(sortRanges seedRanges) maps

let partB (Input (i,maps) : input): string = 
  lookupRanges maps (makeRanges  i) |>
  Iter.of_list |> 
  Iter.map (fun (a,_) -> a) |>
  Iter.min_exn |> string_of_int

let tests = "tests" >::: [golden_test "day5" parser partA "35" ~printer:show_input; 
                          golden_test "day5" parser partB "46" ~printer:show_input;
                          final_answer_test 5 parser partA partB "836040384" "10834440"]
