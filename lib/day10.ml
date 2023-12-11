open Core
open Angstrom
open OUnit
open Utils

type input = char grid 

type showableInput = string list [@@deriving show]

let show_input (Grid i) =
  Iter.of_array i
  |> Iter.map (fun r -> Iter.of_array r |> Iter.to_str)
  |> Iter.to_list |> show_showableInput

let line =
  map (many1 (not_char '\n')) ~f:(fun l -> Iter.of_list l |> Iter.to_array)

let parser =
  map
    (sep_by1 (char '\n') line)
    ~f:(fun l -> Grid (Iter.of_list l |> Iter.to_array))

let negate (x,y) = (-x,-y)

let up = (0,-1)
let down = (0,1)
let left = (-1,0)
let right = (1,0)

let getDirections a = Iter.of_list
  (match a with
   | '|' -> [up;down]
   | '-' -> [left;right]
   | 'L' -> [up;right]
   | 'J' -> [up;left]
   | '7' -> [down;left]
   | 'F' -> [down;right]
   | 'S' -> [up;down;left;right]
   | _ -> [])

let neqdir (x,y) (x2,y2) = x <> x2 || y <> y2

let openInDirection l (c,r) = getDirections l 
   |> Iter.filter (fun (x,y) -> c = x && r = y) 
   |> Iter.length |> (fun a -> a > 0)

let iteratePipe = 
  Iter.unfoldr (fun (grid,dir) -> 
      if Char.(Grid.extract grid = 'S') then
        None
      else
        let next_direction = getDirections (Grid.extract grid) |> 
                             Iter.filter (neqdir dir) |> 
                             Iter.head_exn in
        Some ((grid,next_direction),
              (Grid.move grid next_direction |> 
               Option.value_exn,negate next_direction))
    )  

let enter_from g dir = (iteratePipe (g,dir) |> Iter.length) + 1

let follow_path loc = 
  let exits = [(-1,0);(1,0);(0,1)] in (* dont have last direction because one of the other ones will always match *)
  Iter.of_list exits |> 
    Iter.flat_map (fun dir -> Grid.move loc dir |> 
                            Iter.of_opt |> 
                            Iter.filter (fun l -> openInDirection (Grid.extract l) (negate dir)) |>
                            Iter.map (fun l -> enter_from l (negate dir))) |> Iter.head_exn


let focus_start i = Grid.focus_1true ~f:(fun a -> Char.(a = 'S')) i 

let partA (i : input): string = 
  focus_start i |> follow_path |> (fun a -> a/2) |> string_of_int

let add_direction g dir ar = 
 let (x,y) = Grid.get_location g in 
            if (Array.get (Array.get ar y) x) <> 0 then ()
            else Array.set (Array.get ar y) x (-2);
 match dir with
 | (0,-1) -> let (x,y) = Grid.move g dir |> Option.value_exn |> Grid.get_location in
             Array.set (Array.get ar y) x (-1)
 | (0,1) -> let (x,y) = Grid.get_location g in 
            Array.set (Array.get ar y) x 1
 | _ -> ()
             
let enter_from2 g dir bfield = iteratePipe (g,dir) 
    |> Iter.map (fun (loc,dir) -> add_direction loc dir bfield)

let makePathsDirs grid = 
  let (h,w) = Grid.get_grid grid |> Grid.size in
  let borderEdgeField = Grid.make_sized h w 0 in
  let exits = [(-1,0);(1,0);(0,1)] in
  Iter.of_list exits |>
  Iter.flat_map (fun dir -> 
    Grid.move grid dir |>
    Iter.of_opt |>
    Iter.filter (fun l -> openInDirection (Grid.extract l) (negate dir)) |>
    Iter.map (fun l -> add_direction l dir borderEdgeField; enter_from2 l (negate dir) borderEdgeField)) |> 
  Iter.head_exn |>
  Iter.iter (fun _ -> ());
  borderEdgeField


let rowArea ar = 
  Iter.of_array ar |> Iter.fold 
    (fun (depth,sum) upDown -> 
      if upDown = 0 then 
        if depth <> 0 then 
         (depth,sum + 1) 
        else (depth,sum)
      else if upDown = -2 then (depth,sum) 
           else (depth + upDown,sum)) (0,0) |> (fun (_,sum) -> sum)

let findAreaOfGrid grid = 
  Iter.of_array grid |> Iter.map (fun r -> rowArea r) |> Iter.sum


type rowUpDown = int list [@@deriving show]

let print_row ar = Array.to_list ar |> show_rowUpDown

let partB (grid : input): string =
  let borderStuff = focus_start grid |> makePathsDirs in 
  borderStuff |> findAreaOfGrid |> string_of_int

let tests = "tests" >::: [golden_test "day10a" parser partA "8" ~printer:show_input;
                          golden_test "day10b" parser partA "4" ~printer:show_input;
                          golden_test "day10c" parser partB "8" ~printer:show_input;
                          golden_test "day10d" parser partB "10" ~printer:show_input;
                          golden_test "day10b" parser partB "1" ~printer:show_input;
                          golden_test "day10e" parser partB "4" ~printer:show_input;
                          final_answer_test 10 parser partA partB "6768" "351"]
