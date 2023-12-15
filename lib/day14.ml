open Core
open Angstrom
open OUnit
open Utils

type stone = Rolling | Fixed | Empty [@@deriving show]

type input = stone list list [@@deriving show]

let parser = sep_by1 (char '\n') (many1 (itemParser 
    [('O',Rolling);
     ('#',Fixed);
     ('.',Empty);]))

let sumRow r = 
 let l = List.length r in
 let (sum,_,_) = List.fold r 
  ~f:(fun (sum,at,n) loc -> 
       match loc with
       | Rolling -> (sum+at,at-1,n-1)
       | Fixed -> (sum,n-1,n-1)
       | Empty -> (sum,at,n-1)) ~init:(0,l,l) in
 sum

let partA (i : input): string = 
  List.transpose_exn i |> Iter.of_list |>
  Iter.map (fun r -> sumRow r ) |> Iter.sum |> string_of_int

(* to move rocks in a direction
   sort them into the proper columns with the rocks that don't move, and then sort them
  (probaly can put them in place already sorted) and then compact
 *)

let sort_into_columns i ~size = 
  let ar = Array.create ~len:size [] in
  Iter.iter (fun (r,c,t) -> Array.set ar c ((r,t)::Array.get ar c)) i;
  ar

let rotateCoord r c w h = (c,w-r-1)

let compact rocks = 
 let rec compactgo rs n = 
  match rs with
  | ((_,Rolling)::rs) -> (n,Rolling)::compactgo rs (n+1)
  | ((r,Fixed)::rs) -> (r,Fixed)::compactgo rs (r+1)
  | [] -> []
  | _ -> raise (UtilException "should never happen")
 in compactgo rocks 0
 

let rotate ar ~w ~h : (int * stone) list array = 
  Iter.of_array ar |> 
  Iter.zip_i |> 
  Iter.rev |>
  Iter.flat_map (fun (c,rocks) -> 
    Iter.of_list rocks |>
    Iter.map (fun (r,t) -> 
      let (rr,rc) = rotateCoord r c w h in (rr,rc,t))) |>
  sort_into_columns ~size:h


let compactAll = Array.map ~f:compact

let runCycle rocks ~w ~h = 
  compactAll rocks |>
  rotate ~w:w ~h:h |> (* check *)
  compactAll |>
  rotate ~w:h ~h:w |> (* check *)
  compactAll |>
  rotate ~w:w ~h:h |>
  compactAll |>
  rotate ~w:h ~h:w 

type gridid = int list list [@@deriving show]

module GridId = struct
    type t = gridid
    let t_of_sexp = list_of_sexp (list_of_sexp int_of_sexp)
    let sexp_of_t = sexp_of_list (sexp_of_list sexp_of_int) 
    let compare a b = compare_list (compare_list compare) a b
    let hash l = Hashtbl.hash l
end

let get_load ar ~h = Iter.of_array ar |> 
  Iter.map (fun l -> 
   Iter.of_list l |>
   Iter.map (fun (i,t) -> 
     match t with
     | Rolling -> h - i
     | _ -> 0) |> Iter.sum) |> Iter.sum


let rec applyn n f a = 
  if n <= 0 then a else applyn (n-1) f (f a)

let print_loc ar h w =
  let o = Grid.make_sized h w '.' in
  Iter.of_array ar |> Iter.iteri 
    (fun c l -> List.iter l ~f:
      (fun (r,t) -> match t with
        | Rolling -> Array.set (Array.get o r) c 'O'
        | Fixed -> Array.set (Array.get o r) c '#'
        | _ -> ()));

  Iter.of_array o |> Iter.iter (fun r -> print_endline (Iter.of_array r |> Iter.to_str))

type state = (int * stone) list list [@@deriving show]

let show_thing a = print_endline (show_state (List.of_array a))

let runCycles rocks n = 
  let h = List.length rocks in
  let w = List.nth_exn rocks 0 |> List.length in
  let tbl = Hashtbl.create ~growth_allowed:true ~size:1024 (module GridId) in
  let rec runCyclesGo rocks n c = 
     let postCycle = runCycle rocks ~w:w ~h:h in
     let key = postCycle |> Iter.of_array |> 
          Iter.map (fun a -> Iter.of_list a |> Iter.filter (fun (_,t) -> 
            match t with
            | Rolling -> true
            | _ -> false
          ) |> Iter.map (fun (i,_) -> i) |> Iter.to_list) |> Iter.to_list in

     match Hashtbl.find tbl key with
     | Some a -> 
        let cycleLength = c - a in
        let advanceCountTilln = (n - c) mod cycleLength in
        let final = applyn advanceCountTilln (runCycle ~w:w ~h:h) postCycle in
        get_load final ~h:h
        
     | None ->
         Hashtbl.set tbl ~key:key ~data:c;
         runCyclesGo postCycle n (c+1) in

  let nrocks = Iter.of_list rocks |> Iter.zip_i |> Iter.rev |> Iter.flat_map
       (fun (r,a) -> Iter.of_list a |> Iter.zip_i |> 
                 Iter.filter (fun (c,t) ->
                    match t with
                    | Empty -> false
                    | _ -> true) |> Iter.map (fun (c,t) -> (r,c,t))) |> sort_into_columns ~size:w in

  runCyclesGo nrocks n 1
     


let partB (i : input): string = runCycles i 1000000000 |> string_of_int


let tests = "tests" >::: [golden_test "day14" parser partA "136" ~printer:show_input;
                          golden_test "day14" parser partB "64" ~printer:show_input;]
