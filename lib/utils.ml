open Core
open Angstrom

let rec extend l f = match l with
  | [] -> []
  | (_ :: bs) -> f l :: extend bs f

let str_to_list l = Iter.of_str l |> Iter.to_list

let number = take_while1 Char.is_digit 
              |> map ~f:int_of_string

let verboseRefUtils = ref false

(* input is a filepath output is the string returned *)
let golden_test input parser part output : OUnit.test = TestCase (fun _ -> 
  let file = In_channel.read_all ("goldens/" ^ input) in 
  let data = parse_string ~consume:Consume.Prefix parser file in
  match data with
   | Error s -> print_endline ("parse error: " ^ s)
   | Ok a -> let ans = part a in
             OUnit.assert_equal ~printer:(fun s -> Printf.sprintf "%s" s) output ans)

