open Core
open Angstrom
open OUnit

type input = char list list [@@deriving show]
    
let parser = sep_by (char '\n') (many1 (not_char '\n'))

let rec extend l f = match l with
  | [] -> []
  | (_ :: bs) -> f l :: extend bs f

let str_to_list l = Iter.of_str l |> Iter.to_list

let convertList l = extend l 
  (fun c -> List.fold ~init:c ~f:
               (fun str (l,r) -> if List.is_prefix ~prefix:(str_to_list l) str ~equal:Char.equal 
                                 then str_to_list r 
                                 else str)
            [("one","1");
             ("two","2");
             ("three","3");
             ("four","4");
             ("five","5");
             ("six","6");
             ("seven","7");
             ("eight","8");
             ("nine","9")] |> (fun a -> List.nth_exn a 0)) (*  List.is_prefix ~prefix:"one" c ()) *)

let partA (i : input): string = 
  let ans = Iter.of_list i |> Iter.map 
               (fun w -> let u = List.filter ~f:Char.is_digit w in 
                         let f = List.nth_exn u 0 in
                         let t = List.last_exn u in
                           (Char.get_digit_exn f) * 10 + (Char.get_digit_exn t))
               |> Iter.sum
  in  string_of_int ans

let partB (i : input): string = 
  let ans = Iter.of_list i |> Iter.map 
               (fun w -> let u = List.filter ~f:Char.is_digit (convertList w) in 
                         let f = List.nth_exn u 0 in
                         let t = List.last_exn u in
                           (Char.get_digit_exn f) * 10 + (Char.get_digit_exn t))
               |> Iter.sum
  in  string_of_int ans

let tests = "tests" >::: [TestCase((fun _ -> ()))]
