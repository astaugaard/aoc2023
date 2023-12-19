open Core
open Angstrom
open OUnit
open Utils

type dir = U | D | R | L [@@deriving show]

type input = (dir * int * string) list [@@deriving show]
    
let row = (fun a b c -> (a,b,c)) <$> itemParser [('U',U);('D',D);('R',R);('L',L);]
      <*> (char ' ' *> number)
      <*> (string " (#" *> take_while1 (fun c -> Char.is_alphanum c) <* char ')')

let parser = sep_by1 (char '\n') row

let pointDir dir n = match dir with
    | U -> (0,n)
    | D -> (0,-n)
    | R -> (n,0)
    | L -> (-n,0)

let findArea i = 
  let rec findAreaGo x y ls = 
    match ls with
    | ((dir,n,_)::cs) -> 
        let (dx,dy) = pointDir dir n in
        let da = (x * (y + dy)) - (y * (x + dx)) in
        let (arest,brest) = findAreaGo (x+dx) (y+dy) cs in
        (arest + da,brest + n)
    | [] -> (0,0)
  in
  let (ar,b) = findAreaGo 0 0 i
  in (abs ar,b)

let partA (i : input): string = 
  let (area,borderPoints) = findArea i in
  (area + 2 - borderPoints)/2 + borderPoints |> string_of_int
    

let getDir c = match c with
    | '0' -> R
    | '1' -> D
    | '2' -> L
    | '3' -> U
    | _ -> raise (Utils.UtilException "L+ratio")

let partB (i : input): string = 
  let converted = List.map i ~f:
     (fun (_,_,s) -> let len = String.length s in 
     (getDir (String.get s (len - 1)), int_of_string ("0x" ^ String.sub s ~pos:0 ~len:(len - 1)), "") ) in
  partA converted


let tests = "tests" >::: [golden_test "day18" parser partA "62" ~printer:show_input]
