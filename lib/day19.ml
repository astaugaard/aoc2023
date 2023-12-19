open Core
open Angstrom
open Utils
open OUnit

type expr = string * bool * int * string [@@deriving show]

type input = (string * expr list * string) list * (int * int * int * int) list [@@deriving show]
    
let name = take_while1 Char.is_alpha

let expr = (fun a b c d -> (a,b,c,d)) <$> name <*> (itemParser [
    ('>', true);
    ('<', false);
    ]) <*> number <*> (char ':' *> name)

let workflow = (fun a b c -> (a,b,c)) <$> name <*> (char '{' *> sep_by1 (char ',') expr) <*> ( char ',' *> name <* char '}')

let part = (fun a b c d -> (a,b,c,d)) <$> (string "{x=" *> number) <*> (string ",m=" *> number) <*> (string ",a=" *> number) <*> (string ",s=" *> number <* char '}') 

let parser = (fun a b -> (a,b)) <$> sep_by1 (char '\n') workflow <*> (string "\n\n" *> sep_by1 (char '\n') part)

let checkExpr name gt n (x,m,a,s) = 
  let v = match name with
          | "x" -> x 
          | "m" -> m
          | "a" -> a
          | "s" -> s
          | _ -> raise (UtilException "L+ratio\n") in
  if gt then
    v > n
  else
    v < n


let rec findinExprs exprs v = 
  match exprs with
  | ((name,gt,n,next) ::es) -> if checkExpr name gt n v then Some next else findinExprs es v
  | [] -> None

let rec accepted m loc v = 
  let (exprs,default) = Map.find_exn m loc in 
  let ans = match findinExprs exprs v with
  | Some a -> String.(if a = "A" then true else if a = "R" then false else accepted m a v)
  | None -> String.(if default = "A" then true else if default = "R" then false else accepted m default v)
  in ans

let partA ((workflows,parts) : input): string = 
  let workflowMap = List.map ~f:(fun (n,a,b) -> (n,(a,b))) workflows |> Map.of_alist_exn (module String) in
  Iter.of_list parts |> Iter.filter (accepted workflowMap "in") |> Iter.map (fun (a,b,c,d) -> a + b + c + d ) |> Iter.sum |> string_of_int

let countWays (s,e) = e - s + 1

let numberWays (x,m,a,s) = countWays x * countWays m * countWays a * countWays s

let splitRange gt (s,e) num = 
  if gt then
     if s > num then
        (Some (s,e), None)
     else if e <= num then
        (None, Some (s,e))
     else (Some (num+1,e), Some (s,num))
  else
    if e < num then
       (Some (s,e), None)
    else if s >= num then
       (None,Some (s,e))
    else (Some (s,num - 1),Some (num,e))

let inject n r (x,m,a,s) = 
  match n with
  | "x" -> (r,m,a,s)
  | "m" -> (x,r,a,s)
  | "a" -> (x,m,r,s)
  | "s" -> (x,m,a,r)
  | _ -> raise (UtilException "meow")

let nop () = ()

let rec mapToExprs exprs default (x,m,a,s) c =
  match exprs with
  | ((name,gt,n,next) :: es) ->
       let v = match name with
              | "x" -> x
              | "m" -> m
              | "a" -> a
              | "s" -> s 
              | _ -> raise (UtilException "other thing") in
       let split = splitRange gt v n in
       let goDown = fst split in
       let cont = snd split in
       (match goDown with
       | Some goD -> c (next,inject name goD (x,m,a,s))
       | None -> ());
          
       (match cont with
       | Some co -> let injected = inject name co (x,m,a,s)
                    in mapToExprs es default injected c
       | None -> ()) 
  | [] -> c (default,(x,m,a,s))

let rec combos loc m numbers = 
 match loc with
 | "A" -> numberWays numbers 
 | "R" -> 0
 | _ -> 
   let (exprs,default) = Map.find_exn m loc in
   let locRanges = Iter.from_iter (mapToExprs exprs default numbers) in
   let r = Iter.map (fun (loc,ranges) -> combos loc m ranges) locRanges in
   Iter.sum r

let partB ((workflows,_) : input): string = 
   let flows = List.map ~f:(fun (n,a,b) -> (n,(a,b))) workflows |> Map.of_alist_exn (module String) in 
   let i = combos "in" flows ((1,4000),(1,4000),(1,4000),(1,4000)) in 
   i |> string_of_int

let tests = "tests" >::: [golden_test "day19" parser partA "19114" ~printer:show_input;
                          golden_test "day19" parser partB "167409079868000" ~printer:show_input]
