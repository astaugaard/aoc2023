open Core
open Angstrom
open OUnit
open Utils

type spring = Damaged | Operational | UnKnown [@@deriving show]

type input = (spring list * int list) list [@@deriving show]

let spring = ((fun _ -> Operational) <$> char '.') <|>
             ((fun _ -> Damaged) <$> char '#') <|>
             ((fun _ -> UnKnown) <$> char '?')


let row = (fun a b -> (a,b)) <$> many1 spring <*> (char ' ' *> sep_by1 (char ',') number)

let parser = sep_by1 (char '\n') row 

type printy = int option list [@@deriving show]

let memoGridRec xs ys f x y = 
  let memos = Grid.make_sized ys xs None in
  let rec g x y = 
        if x >= xs || y >= ys then
            f g x y
        else
        match Array.get (Array.get memos y) x with
        | None -> 
            let a = f g x y in
            Array.set (Array.get memos y) x (Some a); a
        | Some a -> a in
  g x y

let isBroken s = match s with
  | Some (Damaged) -> true
  | Some(UnKnown) -> true
  | _ -> false

let getopt ar i = if Array.length ar <= i then None else Some (Array.get ar i)

let checknnext springs i n = 
  List.range 0 n |> 
  Iter.of_list |> 
  Iter.map (fun l -> isBroken (getopt springs (i + l))) |> 
  Iter.fold (&&) true

let isNotBroken s = match s with
 | Some(UnKnown) -> true
 | Some(Operational) -> true
 | None -> true
 | _ -> false

let checkrestfine springs i = 
  List.range i (Array.length springs) |> 
  Iter.of_list |> 
  Iter.map (fun l -> isNotBroken (getopt springs l)) |> 
  Iter.fold (&&) true

let count springs sizes = 
  let h = Array.length springs in
  let w = Array.length sizes in
  memoGridRec w h 
   (fun f si sp ->
     if si >= w then
        if sp >= h then
           1
        else if checkrestfine springs sp then
            1
        else
            0
     else 
     if sp >= h then
        0
     else 
     let n = Array.get sizes si in
     let check () = checknnext springs sp n in
     let fine () = f si (sp + 1) in
     let damaged () = if check () && isNotBroken (getopt springs (sp + n)) then  f (si + 1) (sp + n + 1) else 0 in
     match Array.get springs sp with 
     | UnKnown -> fine () + damaged ()
     | Operational -> fine ()
     | Damaged -> damaged ()
     ) 0 0

let partA (i : input): string = Iter.of_list i |>
  Iter.map (fun (sp,si) -> count (Array.of_list sp) (Array.of_list si)) |>
  Iter.sum |> string_of_int
  

let partB (i : input): string = Iter.of_list i |>
  Iter.map (fun (sp,si) -> count (Array.of_list sp) (Array.of_list si)) |>

let tests = "tests" >::: [golden_test "day12" parser partA "21" ~printer:show_input; final_answer_test 12 parser partA partB "7251" "0"]
