open struct
  let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  ;;
end

module Operators = struct
  let ( -- ) i j = Seq.iterate (( + ) 1) i |> Seq.take_while (fun x -> x < j)
end

include Operators

exception Panic of string

let panic s = raise @@ Panic s
let print_parts = Printf.printf "Part 1: %d\nPart 2: %d\n"
let explode s = List.init (String.length s) (String.get s)
let explodei s = List.init (String.length s) (fun i -> i, String.get s i)
let string_from_chars chars = chars |> List.to_seq |> String.of_seq
let lnth i l = List.nth l i
let split_lines = String.split_on_char '\n'

let array_without_first a =
  match Array.to_list a with
  | _ :: xs -> xs
  | _ -> []
;;

let string_of_int_list l =
  l |> List.map string_of_int |> String.concat "; " |> Printf.sprintf "[%s]"
;;

let list_ain't_empty l =
  match l with
  | [] -> false
  | _ -> true
;;

let split_by pat seq =
  let rec f' curr r list =
    match list with
    | [] -> r
    | x :: [] -> if pat x then r @ [ curr ] else r @ [ curr @ [ x ] ]
    | x :: xs -> if pat x then f' [] (r @ [ curr ]) xs else f' (curr @ [ x ]) r xs
  in
  f' [] [] seq |> List.filter list_ain't_empty
;;

let split_whitespace s =
  s
  |> explode
  |> split_by (fun c ->
    match c with
    | ' ' | '\n' | '\t' | '\r' | '\x0C' -> true
    | _ -> false)
  |> List.map string_from_chars
;;

let rec shared_elements l1 l2 =
  match l1 with
  | [] -> 0
  | x :: xs ->
    let next = shared_elements xs l2 in
    if List.exists (fun a -> a == x) l2 then next + 1 else next
;;

module Regex = struct
  let all_groups regex_str input =
    let rex = Re.Pcre.regexp regex_str in
    Re.Seq.all rex input |> Seq.map Re.Group.all |> Seq.map array_without_first
  ;;

  let first_group rex input =
    all_groups rex input |> Seq.take 1 |> List.of_seq |> List.flatten |> List.hd
  ;;

  let all_matches regex_str input =
    let first g = Re.Group.get g 0 in
    let rex = Re.Pcre.regexp regex_str in
    Re.Seq.all rex input |> Seq.map first
  ;;

  let first_match regex_str input =
    all_matches regex_str input |> Seq.take 1 |> List.of_seq |> List.hd
  ;;
end

open Regex

let get_input () =
  let day, year =
    Array.get Sys.argv 0
    |> all_groups {|d(\d\d?)(\d\d)|}
    |> Seq.take 1
    |> List.of_seq
    |> List.flatten
    |> List.map int_of_string
    |> fun x ->
    match x with
    | [ a; b ] -> a, b
    | _ -> panic "could not parse file [day] and [year]"
  in
  let file_name =
    if Array.length Sys.argv > 1
    then Printf.sprintf "inputs/e%d%d.txt" day year
    else Printf.sprintf "inputs/%d%d.txt" day year
  in
  read_whole_file file_name
;;

let get_input_lines () = get_input () |> split_lines
