open Core
open Angstrom
open OUnit
open Utils

type input = int grid 

type intList = int list [@@deriving show]

let show_input (Grid ar) = Iter.of_array ar |> Iter.map (fun r -> (List.of_array r |> show_intList) ^ "\n") |> Iter.concat_str
    
let row = (fun a -> a |> Iter.of_str |> Iter.map (Char.get_digit_exn) |> Iter.to_array) <$> take_while1 Char.is_digit

let parser = (fun ar -> Grid (Array.of_list ar)) <$> sep_by1 (char '\n') row

type dir = (int*int) [@@deriving sexp]

type location = int * int * int * dir [@@deriving sexp]

let print_loc (x,y,times,(dx,dy)) = Printf.printf "%d %d %d %d %d\n" x y times dx dy

let consCompare compa compb = if compa = 0 then compb else compa

let comparelocs (x,y,z,(dx,dy)) (x1,y1,z1,(dx1,dy1)) = 
   let xs = compare x x1 in
   if xs <> 0 then xs else
   let ys = compare y y1 in
   if ys <> 0 then ys else
   let zs = compare z z1 in
   if zs <> 0 then zs else
   let dxs = compare dx dx1 in
   if dxs <> 0 then dxs else
   compare dy dy1

module Location = struct 
    type t = location
    let compare a b = comparelocs a b
    let t_of_sexp = location_of_sexp
    let sexp_of_t = sexp_of_location
end 

module Distance = struct
    type t = int
    let compare = compare
    let concat a b = a + b
end

module AStar = MakeAstar(Distance) (Location)

let turnDir (dx,_) = 
  if dx <> 0 then
    [(0,-1);(0,1)] |> Iter.of_list
  else [(1,0);(-1,0)] |> Iter.of_list 

let applyDir grid ~old x y times (dx,dy) = 
  let ntimes = if dx = fst old && dy = snd old then times + 1 else 1 in

  let nx = x + dx in
  let ny = y + dy in

  let cost = Grid.get_loc grid (nx,ny) in
  match cost with
  | Some a -> Some (a,(x + dx,y + dy,ntimes,(dx,dy)))
  | None -> None

let nextLocs i (x,y,times,dir) = 
  let nextDirs = if times >= 3 then
     turnDir dir 
  else 
     turnDir dir |> Iter.cons dir
  in Iter.map (applyDir i ~old:dir x y times) nextDirs |> Iter.keep_some

let partA (i : input): string = 
   let (h,w) = Grid.size i in
   AStar.astar ~s:(0,0,0,(0,0)) ~f:(nextLocs i) ~e:(fun (x,y,_,_) -> x >= w - 1 && y >= h - 1) ~dist:(fun (x,y,_,_) -> w - x + h - y) |> string_of_int

let nextLocsU i (x,y,times,dir) = 
  let nextDirs = if times = 0 then
                    Iter.of_list [(0,1);(1,0)]
                 else if times >= 10 then
                    turnDir dir 
                 else (if times >= 4 then
                    Iter.cons dir (turnDir dir)
                 else Iter.singleton dir)
  in Iter.map (applyDir i ~old:dir x y times) nextDirs |> Iter.keep_some

let partB (i : input): string = 
  let (h,w) = Grid.size i in
  AStar.astar ~s:(0,0,0,(0,0)) ~f:(nextLocsU i) ~e:(fun (x,y,e,_) -> (x >= w - 1) && (y >= h - 1) && (e >= 4)) ~dist:(fun (x,y,_,_) -> w - x + h - y) |> string_of_int

let tests = "tests" >::: [golden_test "day17" parser partA ~printer:show_input "102"; golden_test "day17" parser partB ~printer:show_input "94"; golden_test "day17b" parser partB ~printer:show_input "71"]
