open Core
open Utils
open Angstrom
open OUnit

type input = char grid

type showableInput = string list [@@deriving show]

let show_input (Grid i) = Iter.of_array i |> Iter.map (fun r -> Iter.of_array r |> Iter.to_str) |> Iter.to_list |> show_showableInput
   
let line = map (many1 (not_char '\n')) ~f:(fun l -> Iter.of_list l |> Iter.to_array)
 
let parser = map (sep_by1 (char '\n') line) ~f:(fun l -> Grid(Iter.of_list l |> Iter.to_array))

let get_opt l i = let len = Array.length l in 
  if i < len && i >= 0 then Some (Array.get l i) else None

let index i (x,y) = Option.((get_opt i y) >>= (fun r -> get_opt r x) )

let is_part (c : char ) = not (Char.is_digit c) && not Char.(c = '.')

let is_some_digit a = match a with
    | None -> false
    | Some a -> Char.is_digit a

let take_in_direction g (dx,dy) = 
  Iter.iterate succ 0 |> Iter.take_while (fun l -> is_some_digit (Grid.getInDirection g (dx*l,dy*l))) |> 
  Iter.map (fun j -> Option.value_exn (Grid.getInDirection g (dx*j,dy*j))) |> Iter.to_str 

let move_and_take g dir = Option.((Grid.move g dir) >>= (fun l -> let s = take_in_direction l dir in if String.(s = "") then None else Some s))

let singleton a = [a]

let take_in_row g = 
    if Grid.extract g |> Char.is_digit then 
        let toRight = move_and_take g (1,0) in
        let toLeft = take_in_direction g (-1,0) |> String.rev in
        [toLeft ^ Option.value ~default:"" toRight |> int_of_string]
    else let toRight = move_and_take g (1,0) |> Option.map ~f:(fun a -> int_of_string a |> singleton) in
         let toLeft = move_and_take g (-1,0) |> Option.map ~f:(fun a -> String.rev a |> int_of_string |> singleton) in
         Option.value ~default:[] toRight @ Option.value ~default:[] toLeft


let numbersAround (g: char gridComonad) = 
  let toLeft = move_and_take g (-1,0) |> Option.map ~f:(fun a -> String.rev a |> int_of_string |> singleton) in
  let toRight = move_and_take g (1,0) |> Option.map ~f:(fun a -> int_of_string a |> singleton) in
  let top_nums = Option.(Grid.move g (0,1) >>= (fun o -> Some (take_in_row o))) in
  let bottom_nums = Option.(Grid.move g (0,-1) >>= (fun o -> Some (take_in_row o))) in
  Iter.of_list [toLeft;toRight;top_nums;bottom_nums] |> 
  Iter.flat_map (fun a -> Option.value ~default:[] a |> Iter.of_list) |>
  Iter.to_list

let get_loc_sum g = let loc = Grid.extract g in
    if is_part loc then
        let nums = numbersAround g
        in List.fold ~f:(+) ~init:0 nums
    else 0

let partA (i : input): string = 
  Grid.extend_from_grid i get_loc_sum |> Grid.sum |> string_of_int

let is_gear (c : char) = Char.(c = '*')

let get_loc_sumb g = let loc = Grid.extract g in
    if is_gear loc then
        let nums = numbersAround g in
        if List.length nums = 2 then List.fold ~f:( * ) ~init:1 nums else 0
    else 0

let partB (i : input): string = Grid.extend_from_grid i get_loc_sumb |>
                                Grid.sum |> string_of_int

let tests = "tests" >::: [golden_test "day3" parser partA "4361" ~printer:show_input;
                          golden_test "day3" parser partB "467835" ~printer:show_input]
