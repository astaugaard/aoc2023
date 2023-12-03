open Core
open Utils
open Angstrom
open OUnit

type input = char array array

type showableInput = string list [@@deriving show]

let show_input i = Iter.of_array i |> Iter.map (fun r -> Iter.of_array r |> Iter.to_str) |> Iter.to_list |> show_showableInput
   
let line = map (many1 (not_char '\n')) ~f:(fun l -> Iter.of_list l |> Iter.to_array)
 
let parser = map (sep_by1 (char '\n') line) ~f:(fun l -> Iter.of_list l |> Iter.to_array)


let get_opt l i = let len = Array.length l in 
  if i < len && i >= 0 then Some (Array.get l i) else None

let index i (x,y) = Option.((get_opt i y) >>= (fun r -> get_opt r x) )

let is_part (c : char option) = match c with
  | None -> false
  | Some a -> not (Char.is_digit a) && not Char.(a = '.')

let is_some_digit a = match a with
    | None -> false
    | Some a -> Char.is_digit a

let get_loc_sum i (x,y) = let is_start = index i (x-1,y) |> is_some_digit |> not in
  if is_start then 
    (let num = Iter.iterate succ x |> Iter.take_while (fun lx -> index i (lx,y) |> is_some_digit) |> (* refactor later *)
              Iter.map (fun lx -> Option.value_exn (index i (lx,y))) |> Iter.to_str in
    let e = String.length num in
    let near_part = Iter.int_range ~start:(-1) ~stop:1 |> 
      Iter.flat_map (fun r -> Iter.int_range ~start:(-1) ~stop:e |> Iter.map (fun c -> (c,r))) |>
      Iter.map (fun (dx,dy) -> is_part (index i (x+dx,y+dy))) |> 
      Iter.fold (fun (a: bool) (b:bool) -> a || b) false 
    in if near_part && index i (x,y) |> Option.value ~default:'.' |> Char.is_digit 
       then int_of_string num else 0)
  else 0



let partA (i : input): string = 
  let h = Array.length i in 
  let w = Array.length (Array.get i 0) in
  Iter.int_range ~start:0 ~stop:(h-1) |> 
  Iter.flat_map (fun r -> Iter.int_range ~start:0 ~stop:(w-1) |> Iter.map (fun c -> (c,r))) |>
  Iter.map (fun l -> get_loc_sum i l) |>
  Iter.sum |> string_of_int

let is_gear (c : char option) = match c with
  | None -> false
  | Some a -> Char.(a = '*')


let take_in_direction i (x,y) (dx,dy) = 
  let ans = Iter.iterate succ 0 |> Iter.take_while (fun l -> is_some_digit (index i (x+dx*l,y+dy*l))) |> 
            Iter.map (fun j -> Option.value_exn (index i (x+dx*j,y+dy*j))) |> Iter.to_str in
  ans

let take_in_row i (x,y) = if index i (x,y) |> is_some_digit then 
        let toRight = take_in_direction i (x+1,y) (1,0) in
        let toLeft = take_in_direction i (x,y) (-1,0) |> String.rev in
        [toLeft ^ toRight]
    else [take_in_direction i (x-1,y) (-1,0) |> String.rev;
          take_in_direction i (x+1,y) (1,0)]

let get_loc_sum2 i (x,y) =
  if index i (x,y) |> is_gear then 
    let get_left_num =  String.rev (take_in_direction i (x-1,y) (-1,0)) in
    let get_right_num = take_in_direction i (x+1,y) (1,0) in
    let bottom_nums = take_in_row i (x,y+1) in
    let top_nums = take_in_row i (x,y-1) in
    let all_nums = List.concat [[get_left_num];[get_right_num];bottom_nums;top_nums] |> Iter.of_list |>
                   Iter.filter (fun l -> String.length l > 0) |> Iter.to_list in
         if List.length all_nums = 2 then List.fold all_nums ~init:1 ~f:(fun a b -> a * (int_of_string b)) 
         else 0
  else 0


let partB (i : input): string = 
  let h = Array.length i in 
  let w = Array.length (Array.get i 0) in
  Iter.int_range ~start:0 ~stop:(h-1) |> 
  Iter.flat_map (fun r -> Iter.int_range ~start:0 ~stop:(w-1) |> Iter.map (fun c -> (c,r))) |>
  Iter.map (fun l -> get_loc_sum2 i l) |>
  Iter.sum |> string_of_int

let tests = "tests" >::: [golden_test "day3" parser partA "4361" ~printer:show_input;
                          golden_test "day3" parser partB "467835" ~printer:show_input]
