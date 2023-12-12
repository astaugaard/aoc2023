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

let memoGridRec h w f x y= 
  let memos = Grid.make_sized h w None in
  let rec g x y = 
        match Array.get (Array.get memos y) x with
        | None -> 
            let a = f g x y in
            Array.set (Array.get memos y) x (Some a); a
        | Some a -> a in
  g x y

let count springs sizes = 
  let h = Array.length springs in
  let w = Array.length sizes in
  memoGridRec h w (fun f x y -> _) 0 0
   

let partA (_ : input): string = "not yet implemented"

let partB (_ : input): string = "not yet implemented"

let tests = "tests" >::: [TestCase((fun _ -> ()))]
