let input = Advent.get_input_lines ()

type card =
  { index : int
  ; winners : int list
  ; own : int list
  }

let _string_of_card { index; winners; own } =
  Printf.sprintf
    "%d : %s | %s"
    index
    (Advent.string_of_int_list winners)
    (Advent.string_of_int_list own)
;;

(** Square *)
let rec ( ** ) a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = a ** (n / 2) in
    b * b * if n mod 2 = 0 then 1 else a
;;

let card_value c = Advent.shared_elements c.winners c.own

exception Panic of string

let parse_line line =
  let s1 =
    match String.split_on_char ':' line with
    | [ a; b ] -> a, b
    | _ -> raise @@ Panic (Printf.sprintf "could not split \"%s\" by ':'" line)
  in
  let index, numbers =
    s1 |> fun (a, b) -> a |> Advent.split_whitespace |> Advent.lnth 1 |> int_of_string, b
  in
  let winners, own =
    match
      numbers
      |> String.split_on_char '|'
      |> List.map Advent.split_whitespace
      |> List.map (fun l -> List.map int_of_string l)
    with
    | [ a; b ] -> a, b
    | _ -> raise @@ Panic "could not split '|'"
  in
  { index; winners; own }
;;

let cards = input |> List.map parse_line

let part1 =
  cards
  |> List.map (fun c ->
    match card_value c with
    | 0 -> 0
    | n -> 2 ** (n - 1))
  |> List.fold_left ( + ) 0
;;

let part2 =
  let folder wins copies =
    List.mapi (fun i (a, b) -> if i < wins then a + copies, b else a, b)
  in
  let rec f' = function
    | [] -> []
    | (a, b) :: xs -> (a, b) :: f' (folder b a xs)
  in
  cards
  |> List.map (fun x -> 1, card_value x)
  |> f'
  |> List.fold_left (fun acc (a, _) -> acc + a) 0
;;

let () = Advent.print_parts part1 part2

(*
   1
   2
   4
   8
   14
   1
*)
