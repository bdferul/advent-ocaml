let input = Advent.get_input ()

let all_nums s =
  let rex = Re.Pcre.regexp {|\d+|} in
  let r =
    match Re.all rex s with
    | [] -> Advent.panic "no integers found"
    | xs -> List.map (fun g -> Re.Group.get g 0 |> int_of_string) xs
  in
  r
;;

let seeds =
  let open Advent.Regex in
  first_match {|: (.+)|} input
  |> all_matches {|\d+|}
  |> Seq.map int_of_string
  |> List.of_seq
;;

let maps =
  let open Advent.Regex in
  let r1 = {|:\n((?:[\S\s])+?)(?:\n\n)|} in
  let r2 = {|(\d+) (\d+) (\d+)|} in
  let parse_match x =
    all_groups r2 x
    |> Seq.map (List.map all_nums)
    |> Seq.map List.flatten
    |> Seq.map (fun x ->
      match x with
      | [ a; b; c ] -> a, b, c
      | xs ->
        let () = print_endline (Advent.string_of_int_list xs) in
        Advent.panic "not exactly 3")
    |> List.of_seq
  in
  all_matches r1 input |> Seq.map parse_match |> List.of_seq
;;

let crspnd mapl seed =
  let r =
    mapl
    |> List.find_map (fun (dst, src, range) ->
      if src <= seed && seed <= src + range then Some (seed - src + dst) else None)
    |> Option.value ~default:seed
  in
  let () = Printf.printf "%d -> %d\n" seed r in
  r
;;

let mapped =
  seeds
  |> List.map (fun seed ->
    let r = List.fold_left (fun acc mapl -> crspnd mapl acc) seed maps in
    let () = print_newline () in
    r)
;;

let () = Printf.printf "mapped len: %d\n" (List.length mapped)

let part1 =
  List.fold_left (fun acc x -> if acc > x then x else acc) (List.nth mapped 0) mapped
;;

let () =
  let open Base in
  ()
;;

open Base
open Core

let () = printf "part 1: %d\n" part1
