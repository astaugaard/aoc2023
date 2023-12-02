open Core
open Angstrom
open OUnit
open Utils
                   (* r   g   b *)
type colors = Colors of int * int * int [@@deriving show]

type game = Game of int * colors list [@@deriving show]

type input = game list [@@deriving show]

let colorsParser = (fun l -> List.fold l ~init:(Colors (0,0,0)) ~f:(fun acc b -> b acc)) <$> sep_by (string ", ") 
                   ((fun n c -> c n) <$> number <*> 
                       (char ' ' *> 
                       (map ~f:(fun _ n (Colors (r,g,b)) -> Colors (n+r,g,b)) (string "red") <|>
                        map ~f:(fun _ n (Colors (r,g,b)) -> Colors (r,n+g,b)) (string "green") <|>
                        map ~f:(fun _ n (Colors (r,g,b)) -> Colors (r,g,n+b)) (string "blue"))))

let gameParser = (fun a b -> Game (a,b)) <$> (string "Game " *> number) 
                                         <*> (string ": " *> sep_by1 (string "; ") colorsParser)

let parser = sep_by1 (char '\n') gameParser

let partA (i : input): string = Iter.of_list i |> 
    Iter.filter (fun (Game (_,colors)) -> Iter.of_list colors |> 
                                   Iter.filter (fun (Colors(r,g,b)) -> r > 12 || g > 13 || b > 14) |> 
                                   Iter.is_empty) |>
    Iter.map (fun (Game (i,_)) -> i) |> Iter.sum |> string_of_int

let max_of l ~f = Iter.of_list l |> Iter.map f |> Iter.max_exn

let partB (i : input): string = Iter.of_list i |>
    Iter.map (fun (Game (_,colors)) -> max_of ~f:(fun (Colors(r,_,_)) -> r) colors 
                                     * max_of ~f:(fun (Colors(_,g,_)) -> g) colors 
                                     * max_of ~f:(fun (Colors(_,_,b)) -> b) colors) |> 
    Iter.sum |> string_of_int

let tests = "tests" >::: [golden_test "day2" parser partA "8" ~printer:show_input; 
                          golden_test "day2" parser partB "2286" ~printer:show_input]
