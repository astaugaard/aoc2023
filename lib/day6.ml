open Core
open Angstrom
open OUnit
open Utils

type input = Input of int list * int list [@@deriving show]
    
let parser = (fun a b -> Input (a,b)) <$> (string "Time: " *> many1 (char ' ') *> sep_by1 (many1 (char ' ')) number) <*> (string "\nDistance:" *> many1 (char ' ') *> sep_by1 (many1 (char ' ')) number)


(* d = tT - t^2 *)
(* -t^2 + tT - d = 0 
t = (T +- sqrt(T^2 - 4 * d))/2

*)
let winningTimes (time:int) (distance:int)  = 
   let rt = Float.sqrt(float_of_int (time * time - 4 * distance)) in
   let maxT = int_of_float (Float.round_up((float_of_int time +. rt) /. 2.)) - 1 in 
   let minT = int_of_float (Float.round_down((float_of_int time -. rt) /. 2.)) + 1 in 
   (minT,maxT)

let possibleTimes (t,distance) = 
  let (minW,maxW) = winningTimes t distance in
  maxW - minW + 1

let partA (Input (times,distances) : input): string 
  = List.zip_exn times distances |> 
    Iter.of_list |>
    Iter.map possibleTimes |> 
    Iter.fold (fun acc b -> acc * b) 1 |>
    string_of_int

let partB (Input (times,distances)  : input): string = 
    let totalTime = List.map ~f:string_of_int times |> String.concat |> int_of_string in
    let totalDistance = List.map ~f:string_of_int distances |> String.concat |> int_of_string in
    possibleTimes (totalTime,totalDistance) |> string_of_int

let tests = "tests" >::: [golden_test "day6" parser partA "288" ~printer:show_input; 
                          golden_test "day6" parser partB "71503"; 
                          final_answer_test 6 parser partA partB "440000" "26187338"]
