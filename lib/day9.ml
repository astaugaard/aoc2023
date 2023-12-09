open Core
open Angstrom
open OUnit
open Utils

type input = int list list [@@deriving show]
    
let parser = sep_by1 (char '\n') (sep_by1 (char ' ') negativeNumber)

let factorial n = 
  let rec facRec n acc = if n = 0 then acc else facRec (n-1) (n*acc) in
  facRec n 1

(*  let lagrangeValue data n =  *)
(*    let len = List.length data in *)
(*    let prods = List.mapi data ~f:(fun i _ -> n - i - 1) in *)
(*    let product = List.fold prods ~init:1 ~f:(fun a b -> a * b) in *)
(*    let startSign = if len mod 2 = 1 then 1 else -1 in *)
(*    let invWeights = List.range 1 len  *)
(*          |> Iter.of_list  *)
(*          |> Iter.scan (fun a i -> -a * i/(len - i)) (startSign * factorial (len - 1))  *)
(*          |> Iter.to_list in *)
(*    let zipped = List.zip_exn invWeights data in  *)
(*    let els = List.mapi zipped ~f:(fun v (w,d) ->  *)
(*        let i = v + 1 in  *)
(*        ((float_of_int product /. (float_of_int n -. float_of_int i)) /. float_of_int w) *. float_of_int d *)
(*        ) in  *)
(*    List.fold ~init:0.0 ~f:(fun a b -> a +. b) els |> int_of_float  *)

let predictNext d = 
  let lenD = List.length d in
  let lenDf = float_of_int lenD in
  let sign = if lenD mod 2 = 1 then 1.0 else -1.0 in
  let ak = List.range 1 lenD |> Iter.of_list  
         |> Iter.scan (fun an k -> -1.0 *. an*.(lenDf -. float_of_int k +. 1.)/.(float_of_int k)) sign |> Iter.to_list in
  List.map2_exn d ak ~f:(fun d a -> float_of_int d *. a) |> List.fold ~init:0.0 ~f:(+.) |> int_of_float

let derivative i = 
  let a = Sequence.memoize (Sequence.of_list i) in
  let b = Sequence.drop a 1 in
  Sequence.zip a b |> Sequence.map ~f:(fun (a,b) -> b - a) |> Sequence.to_list

let derivs i = 
  Iter.iterate derivative i |> Iter.take_while (fun a -> not (List.fold ~init:true ~f:(fun ac b -> b = 0 && ac) a)) |> Iter.to_list

let predicNext1 i = derivs i |>
  List.fold_right ~f:(fun a acc -> List.last_exn a + acc) ~init:0

(*  let predicNext i =  *)
(*    let n = List.length i + 1 in *)
(*     lagrangeValue i n *)

let partA (i : input): string = 
   let a = Iter.of_list i |> Iter.map (fun a -> predictNext a)
   in a |> Iter.sum |> string_of_int

let predicBefore i = derivs i |>
  List.fold_right ~f:(fun a acc -> List.nth_exn a 0 - acc) ~init:0

let partB (i : input): string = 
   let a = Iter.of_list i |> Iter.map predicBefore
   in a |> Iter.sum |> string_of_int

let tests = "tests" >::: [golden_test "day9" parser partA "114" ~printer:show_input;
                          golden_test "day9" parser partB "2" ~printer:show_input ]

