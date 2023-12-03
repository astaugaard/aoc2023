open Core
open Angstrom

let rec extend l f = match l with
  | [] -> []
  | (_ :: bs) -> f l :: extend bs f

let str_to_list l = Iter.of_str l |> Iter.to_list

let number = take_while1 Char.is_digit 
              |> map ~f:int_of_string

let testsVerbose = ref false

(* input is a filepath output is the string returned *)
let golden_test input parser ?printer part output : OUnit.test = TestCase (fun _ -> 
  let file = In_channel.read_all ("goldens/" ^ input) in 
  let data = parse_string ~consume:Consume.Prefix parser file in
  match data with
   | Error s -> OUnit.assert_failure ("parse error: " ^ s)
   | Ok a -> (match printer with
             | Some prin when !testsVerbose -> 
                 ANSITerminal.print_string [ANSITerminal.blue] ("==== golden " ^ input ^ " ====\n");
                 print_endline (prin a);
             | _ -> ());
             let ans = part a in
             OUnit.assert_equal ~printer:(fun s -> Printf.sprintf "%s" s) output ans)

type 'a gridComonad = GridComonad of 'a array array * int * int

let extendG (GridComonad (l,x,y)) f = 
  GridComonad (Array.mapi l 
    ~f:(fun ri r -> Array.mapi r 
           ~f:(fun ci _ -> f (GridComonad (l,ri,ci)))),x,y)

let focus (GridComonad (l,_,_)) (x,y) = GridComonad (l,x,y)

let extract (GridComonad (l,x,y)) = 
  let get_opt l i = let len = Array.length l in 
                    if i < len && i >= 0 then Some (Array.get l i) 
                    else None in

  Option.((get_opt l y) >>= (fun r -> get_opt r x) ) 

  
let getInDirection (GridComonad (l,x,y)) (dx,dy) = 
  focus (GridComonad (l,x,y)) (x+dx,y+dy) |> extract
