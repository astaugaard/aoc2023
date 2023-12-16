open Core
open Angstrom
open OUnit
open Utils

module T = Domainslib.Task

type mirror = AngledNW | AngledNE | SplitNS | SplitEW | Empty [@@deriving show]
type input =  mirror grid 

let show_input grid = 
  let (w,h) = Grid.size grid in
  let ar = Grid.make_sized w h '.' in
  Grid.iteri grid ~f:(fun x y v ->
    let set_loc c = Array.set (Array.get ar y) x c in
    match v with
    | Empty -> ()
    | AngledNE -> set_loc '\\'
    | AngledNW -> set_loc '/'
    | SplitEW -> set_loc '-'
    | SplitNS -> set_loc '|'
   );
  Iter.of_array ar |> Iter.map (fun r -> (Iter.of_array r |> Iter.to_str) ^ "\n") |> Iter.concat_str
    
let row = Array.of_list <$> many1 (itemParser 
  [('\\',AngledNE);
   ('/',AngledNW);
   ('-',SplitEW);
   ('|',SplitNS);
   ('.',Empty);
  ])

let add (x,y) (dx,dy) = (dx+x,dy+y)

let deflectDirections mirror (x,y) = 
  match mirror with
  | Empty  -> [(x,y)]
  | AngledNE -> [(y,x)]
  | AngledNW -> [(-y,-x)]
  | SplitEW -> if x = 0 then [(-1,0);(1,0)] else [(x,y)]
  | SplitNS -> if y = 0 then [(0,-1);(0,1)] else [(x,y)] 

let atAngle mirror = 
  match mirror with
  | AngledNE -> true
  | AngledNW -> true
  | _ -> false

let simulate grid sloc sdir = 
   let (w,h) = Grid.size grid in

   let out_of_range (x,y) = x >= w || y >= h || x < 0 || y < 0 in

   let visitedLocs = Grid.make_sized w h (false,false) in

   let get_mirror (x,y) = Grid.get_loc_exn grid (x,y) in

   let try_visit (x,y) (dx,dy) visit = 
     let row = Array.get visitedLocs y in
     let (vx,vy) = Array.get row x in
     if not vx && not vy then
        visit (x,y,dx,dy)
     else ();
     
     let nvx = vx || dx <> 0 in
     let nvy = vy || dy <> 0 in
     
     Array.set row x (nvx,nvy);
     Bool.(nvx <> vx || nvy <> vy)
     in

   let rec simulatego loc dir visit = 
     let mirror = get_mirror loc in 
     let angledMirror = atAngle mirror in
     if try_visit loc dir visit || angledMirror then
         let dirs = deflectDirections mirror dir in
         List.iter ~f:(fun d -> let newloc = add loc d in 
               if out_of_range newloc then () 
               else simulatego (add loc d) d visit) dirs
     else () in
   Iter.from_iter (simulatego sloc sdir)
    

let parser = (fun a -> Grid (Array.of_list a)) <$> sep_by1 (char '\n') row

let partA (i : input): string = 
  let locs = simulate i (0,0) (1,0) in 
  Iter.length locs |> string_of_int

    
let rec mapRangeReduce pool start amount ~f ~comb ~init = if amount < 3 
  then List.range start (start + amount) |> List.map ~f:f |> List.fold ~init:init ~f:comb
  else let o = T.async pool (fun _ -> mapRangeReduce pool start (amount/2) ~f:f ~comb:comb ~init:init) in 
       let v = T.async pool (fun _ -> mapRangeReduce pool (start + amount/2) (amount - amount/2) ~f:f ~comb:comb ~init:init) in
       comb (T.await pool o) (T.await pool v) 
  
let parPartB i pool = 
  let (w,h) = Grid.size i in
  let rowMaxs = T.async pool (fun _ -> 
    mapRangeReduce pool 0 h ~f:(fun ind -> 
        let fromLeft = T.async pool (fun _ -> simulate i (0,ind) (1,0) |> Iter.length) in
        let fromRight = T.async pool (fun _ -> simulate i (w-1,ind) (-1,0) |> Iter.length) in
        max (T.await pool fromLeft) (T.await pool fromRight)) ~comb:max ~init:0) in

  let colMaxs = T.async pool (fun _ -> 
    mapRangeReduce pool 0 h ~f:(fun ind -> 
        let fromTop = T.async pool (fun _ -> simulate i (ind,0) (0,-1) |> Iter.length) in
        let fromBottom = T.async pool (fun _ -> simulate i (ind,h-1) (0,1) |> Iter.length) in
        max (T.await pool fromTop) (T.await pool fromBottom)) ~comb:max ~init:0) in
  
  max (T.await pool rowMaxs) (T.await pool colMaxs)


let partB (i : input): string = 
  let numThreads = 6 in
  let pool = T.setup_pool ~num_domains:(numThreads - 1) () in
  let res = T.run pool (fun _ -> parPartB i pool) in
   
  string_of_int res

let tests = "tests" >::: [golden_test "day16" parser partA "46" ~printer:show_input; 
                          golden_test "day16" parser partB "51" ~printer:show_input]
