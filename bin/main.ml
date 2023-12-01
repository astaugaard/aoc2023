open Core;;
open Angstrom


(* let number = take_while1 Char.is_digit 
              |> map ~f:int_of_string *)

let parser = sep_by (char '\n') (many1 (not_char '\n'))

(*  let show_data = Format.asprintf "@[%a@]" [%show: int list list] *)

(*  let intSum (v: int list): int = List.fold v ~init:0 ~f:(+) *)

exception Foo of string

let fromSome a =  match a with
                   | None -> raise (Foo "is none")
                   | Some v -> v

(* could replace this with a fold *)
let convertString s = String.substr_replace_all ~pattern:"one" ~with_:"1" s |>     
                      String.substr_replace_all ~pattern:"two" ~with_:"2" |>     
                      String.substr_replace_all ~pattern:"three" ~with_:"3" |>     
                      String.substr_replace_all ~pattern:"four" ~with_:"4" |>     
                      String.substr_replace_all ~pattern:"five" ~with_:"5" |>     
                      String.substr_replace_all ~pattern:"six" ~with_:"6" |>     
                      String.substr_replace_all ~pattern:"seven" ~with_:"7" |>     
                      String.substr_replace_all ~pattern:"eigh" ~with_:"8" |>     
                      String.substr_replace_all ~pattern:"nine" ~with_:"9"

let convertList l = Iter.of_list l |> Iter.to_str |> convertString |> Iter.of_str |> Iter.to_list

let () = 
 let file: string = In_channel.with_file ~binary:false "data" ~f:In_channel.input_all in 
 let data = parse_string ~consume:Consume.Prefix parser file 
 in match data with
     | Error e -> print_endline ("error" ^ e)
     | Ok v -> let ans = 
                    Iter.of_list v |> Iter.map 
                        (fun w -> let u = List.filter ~f:Char.is_digit (convertList w) in 
                           let f = List.nth u 0 |> fromSome in
                           let t = List.last u |> fromSome in
                           fromSome (Char.get_digit f) * 10 + fromSome (Char.get_digit t))
                             |> Iter.sum
               in  string_of_int ans |> print_endline
