open Core
open Angstrom
open OUnit
open Utils

type input = bool list list list [@@deriving show]
    
let row = many1 ((const true <$> char '#') <|> (const false <$> char '.'))

let grid = (fun ar -> ar) <$> sep_by1 (char '\n') row

let parser = sep_by1 (string "\n\n") grid

type reflection = Col of int | Row of int

let list_extend ar ~f = 
 let rec list_extend_go ar acc f =  
    match ar with
    | (b::bs) -> let nacc = b::acc in
        f nacc bs :: list_extend_go bs nacc f
    | [] -> [] in
 list_extend_go ar [] f

let rec check_sym a b = match (a,b) with
  | ((a::ls),(b::bs)) -> if List.zip_exn a b |> List.fold ~f:(fun acc (a,b) -> Bool.(a = b) && acc) ~init:true 
                         then check_sym ls bs else false
  | _ -> true

let try_find_rows (a: bool list list) = list_extend a 
  ~f:(fun a b -> if List.length a > 0 && List.length b > 0 then 
        check_sym a b
      else false) |> List.mapi ~f:(fun i a -> (i,a))|> List.find ~f:(fun (_,a) -> a) |>
   Option.map ~f:(fun (i,_) -> i)

let try_find_cols a = List.transpose_exn a |> try_find_rows

let findReflection a = 
  match try_find_cols a with
  | Some c -> Col c
  | None -> match try_find_rows a with
     | Some r -> Row r
     | None -> raise (UtilException "should never happen")

let partA (i : input): string = List.map 
  ~f:(fun a -> 
     match findReflection a with
     | Row r -> (r+1) * 100
     | Col c -> (c+1)) i |> List.fold ~f:(+) ~init:0 |> string_of_int

let rec check_sym a b n = match (a,b) with
  | ((a::ls),(b::bs)) -> let diff = List.zip_exn a b |> List.fold ~f:(fun acc (a,b) -> (if Bool.(a = b) then 0 else 1) + acc) ~init:n in
                         if diff > 1 then 
                            false
                         else check_sym ls bs diff
  | _ -> n = 1

let try_find_rows (a: bool list list) = list_extend a 
  ~f:(fun a b -> if List.length a > 0 && List.length b > 0 then 
        check_sym a b 0
      else false) |> List.mapi ~f:(fun i a -> (i,a))|> List.find ~f:(fun (_,a) -> a) |>
   Option.map ~f:(fun (i,_) -> i)

let try_find_cols a = List.transpose_exn a |> try_find_rows

let findReflectionPart a = 
  match try_find_cols a with
  | Some c -> Col c
  | None -> match try_find_rows a with
     | Some r -> Row r
     | None -> raise (UtilException "should never happen")

let partB (i : input): string = List.map 
  ~f:(fun a -> 
     match findReflectionPart a with
     | Row r -> (r+1) * 100
     | Col c -> (c+1)) i |> List.fold ~f:(+) ~init:0 |> string_of_int

let eqofref a b = match (a,b) with
    | (Col a,Col b) -> a = b
    | (Row a,Row b) -> a = b
    | _ -> false

let tests = "tests" >::: [
    "horizontal" >:: (fun _ -> assert_equal (findReflection [[true;false;true;true;false;false;true;true;false;];
[false;false;true;false;true;true;false;true;false;];
[true;true;false;false;false;false;false;false;true;];
[true;true;false;false;false;false;false;false;true;];
[false;false;true;false;true;true;false;true;false;];
[false;false;true;true;false;false;true;true;false;];
[true;false;true;false;true;true;false;true;false;];]) (Col 4))]


