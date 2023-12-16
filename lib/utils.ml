open Core
open Angstrom
open OUnit


exception UtilException of string

let rec extend l f = match l with
  | [] -> []
  | (_ :: bs) -> f l :: extend bs f

let str_to_list l = Iter.of_str l |> Iter.to_list

let number = take_while1 Char.is_digit 
              |> map ~f:int_of_string

let negativeNumber = (fun a b -> if a then -b else b) <$> option false (const true <$> char '-') <*> number

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

let final_answer_test day parser parta partb outputa outputb  : OUnit.test = 
    let file = In_channel.read_all ("inputs/day" ^ string_of_int day) in 
    let data = parse_string ~consume:Consume.Prefix parser file in
    let testthing d p o = match d with
        | Error s -> assert_failure ("parse error: " ^ s)
        | Ok a -> let ans = p a in OUnit.assert_equal ~printer:(fun s -> Printf.sprintf " %s" s) o ans in
    "real answers" >::: [
        "part a" >:: (fun _ -> testthing data parta outputa);
        "part b" >:: (fun _ -> testthing data partb outputb)
    
    ]

type 'a grid = Grid of 'a array array

type 'a gridComonad = GridComonad of 'a grid * int * int

module Grid = struct
  let make_sized h w i = 
    let one_row = Array.create ~len:w i in
    Array.init h ~f:(fun _ -> Array.copy one_row)
  
  let get_grid (GridComonad (g,_,_)) = g

  let size (Grid g) = let h = Array.length g in 
    if h > 0 then (h,Array.get g 0 |> Array.length) else (0,0)

  let fold ~f ~init (Grid g)= 
    Array.fold g ~init:init ~f:(fun acc r -> Array.fold r ~init:acc ~f:f)

  let sum g = fold ~f:(+) ~init:0 g

  let get_loc(Grid l) (x,y) = 
    let get_opt l i = let len = Array.length l in 
                      if i < len && i >= 0 then Some (Array.get l i) 
                      else None in
  
    Option.((get_opt l y) >>= (fun r -> get_opt r x) ) 


  let get_loc_exn g (x,y) = 
    match get_loc g (x,y) with
       | None -> raise (UtilException "no item at location")
       | Some a -> a

  let map_gridi (Grid l) f = Grid 
     (Array.mapi l 
        ~f:(fun ri r -> Array.mapi r 
            ~f:(fun ci c -> f ci ri c)))
  
  let extend_from_grid g f = map_gridi g (fun c r _ -> f (GridComonad (g,c,r)))

  let extend (GridComonad (g,x,y)) f = 
    GridComonad (extend_from_grid g f,x,y)

  let focus (GridComonad (l,_,_)) (x,y) = let (h,w) = size l in
    if x < w && x >= 0 && y < h  && y >= 0 then Some (GridComonad (l,x,y)) else None

  let move (GridComonad (l,x,y)) (dx,dy) = focus (GridComonad (l,x,y)) (dx+x,dy+y)

  let extract (GridComonad (g,x,y)) = get_loc_exn g (x,y)

  let getInDirection g (dx,dy) = 
    match move g (dx,dy) with
    | Some a -> Some (extract a)
    | None -> None

  let take_while_in_direction g dir ~f = 
     Iter.unfoldr Option.(fun g -> g >>= fun g' ->
                                   (extract g' |> f) >>= fun v -> 
                                   Some (v,move g' dir )) (Some g)

  let focus_1true (Grid a) ~f = 
    let (x,y,_) = Iter.of_array a |> Iter.zip_i |> 
        Iter.flat_map (fun (y,r) -> Iter.of_array r |> 
                                Iter.mapi (fun x a -> (x,y,a)) |> 
                                Iter.filter (fun (_,_,s) -> f s)) |>
        Iter.head_exn in
   GridComonad (Grid a,x,y) 

  let get_location (GridComonad (_,x,y)) = (x,y)

  let iteri (Grid a) ~f = Iter.of_array a |> Iter.iteri 
   (fun y r -> Iter.of_array r |> Iter.iteri (fun x v -> f x y v))
end

let rec itemParser ls =
  match ls with
  | ((c,v) :: list) -> ((fun _ -> v) <$> char c) <|> itemParser list
  | [] -> fail "no characters matched"
  
