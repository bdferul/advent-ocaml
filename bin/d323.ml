open Advent

let input = Advent.get_input_lines ()

type xy =
  { x : int
  ; y : int
  }

type number =
  { value : int
  ; start : xy
  ; len : int
  }

type symbol =
  { symbol : char
  ; position : xy
  }

type line_item =
  | Symbol of symbol
  | Number of number

let parse_line line y =
  let symbol_from_ic (x, symbol) = Symbol { symbol; position = { x; y } } in
  let li_list_from_ic_list l =
    let s = l |> List.map snd |> string_from_chars in
    match int_of_string_opt s with
    | Some value ->
      let x = l |> List.hd |> fst in
      [ Number { value; start = { x; y }; len = List.length l } ]
    | None -> l |> List.map symbol_from_ic
  in
  line |> explodei |> split_by (fun (_, c) -> c == '.') |> List.map li_list_from_ic_list
;;

let get_numbers y line =
  line
  |> explodei
  |> split_by (fun (_, c) ->
    match c with
    | '0' .. '9' -> false
    | _ -> true)
  |> List.map (fun x ->
    let value =
      List.fold_left (fun acc (_, c) -> acc ^ string_from_chars [ c ]) String.empty x
      |> int_of_string
    in
    { value
    ; start =
        (match List.nth x 0 with
         | i, _ -> { x = i; y })
    ; len = List.length x
    })
;;

let get_symbols y line =
  line
  |> String.to_seqi
  |> Seq.filter (fun (_, c) ->
    match c with
    | '0' .. '9'
    | '.' ->
      false
    | _ -> true)
  |> Seq.map (fun (x, symbol) -> { symbol; position = { x; y } })
  |> Seq.fold_left (fun acc x -> x :: acc) []
;;

let all_numbers =
  input
  |> List.mapi (fun i x -> get_numbers i x)
  |> List.fold_left (fun acc x -> acc @ x) []
;;

let all_symbols =
  input
  |> List.mapi (fun i x -> get_symbols i x)
  |> List.fold_left (fun acc x -> acc @ x) []
;;

let symbol_tounching_number symbol number =
  symbol.position.x >= number.start.x - 1
  && symbol.position.y >= number.start.y - 1
  && symbol.position.x <= number.start.x + number.len
  && symbol.position.y <= number.start.y + 1
;;

let numbers_touching_symbols =
  all_numbers
  |> List.filter (fun n -> List.exists (fun s -> symbol_tounching_number s n) all_symbols)
;;

let part1 = numbers_touching_symbols |> List.fold_left (fun acc x -> acc + x.value) 0

let part2 =
  all_symbols
  |> List.filter (fun s -> s.symbol == '*')
  |> List.map (fun s ->
    numbers_touching_symbols |> List.filter (fun n -> symbol_tounching_number s n))
  |> List.map (fun ns ->
    match ns with
    | [ a; b ] -> a.value * b.value
    | _ -> 0)
  |> List.fold_left (fun acc x -> acc + x) 0
;;

let () = Printf.printf "part1: %d\npart 2: %d\n" part1 part2
