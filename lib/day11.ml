(* open Core *)
open Angstrom
open OUnit
open Utils

type input = bool array array 

let show_input ar = let strrows = Array.map 
    (fun r -> Iter.of_array r |> Iter.map (fun a -> if a then '#' else ' ') |> Iter.to_str) 
    ar in
  Array.fold_right (fun row str -> row ^ "\n" ^ str) strrows ""
    
let row = Array.of_list <$> many1 (((fun _ -> true) <$> char '#') <|> 
                                   ((fun _ -> false) <$> char '.') )

let parser = Array.of_list <$> sep_by1 (char '\n') row

let mapindexof f i ar = Array.get ar i |> f |> Array.set ar i 

let countDistance timesLarger ar = 
  let (_,_,s) = Iter.of_array ar |> 
  Iter.fold (fun (distances,count,sum) c -> 
    let newDistances = distances + if c = 0 then count*timesLarger else count in 
    let newCount = count + c in 
    let newSum = sum + newDistances * c in
    (newDistances,newCount,newSum)) (0,0,0) in
  s


let countInColAndRow i = 
  let h = Array.length i in
  let w = Array.get i 0 |> Array.length in 
  let galaxyRowCount = Array.make h 0 in
  let galaxyColCount = Array.make w 0 in
  Iter.of_array i |> Iter.zip_i |> 
  Iter.flat_map(fun (rn,r) -> 
    Iter.of_array r |> 
    Iter.mapi (fun cn a -> (rn,cn,a)) |> 
    Iter.filter (fun (_,_,a) -> a)) |>
  Iter.map (fun (r,c,_) -> mapindexof succ r galaxyRowCount; 
                           mapindexof succ c galaxyColCount) |>
  Iter.iter (fun _ -> ());
  (galaxyColCount,galaxyRowCount)

let partA (i : input): string = 
  let (galaxyColCount,galaxyRowCount) = countInColAndRow i in
  string_of_int (countDistance 2 galaxyColCount + countDistance 2 galaxyRowCount)

let sized i a = 
  let (galaxyColCount,galaxyRowCount) = countInColAndRow i in
  string_of_int (countDistance a galaxyColCount + countDistance a galaxyRowCount)

let partB (i : input): string = sized i 1000000

let tests = "tests" >::: [golden_test "day11" parser partA "374" ~printer:show_input;
                          golden_test "day11" parser (fun i -> sized i 10) "1030" ~printer:show_input;
                          golden_test "day11" parser (fun i -> sized i 100) "8410" ~printer:show_input;]
