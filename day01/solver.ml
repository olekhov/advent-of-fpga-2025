let parse_line line =
  let line = String.trim line in
  let dir = line.[0] in
  let amount = int_of_string (String.sub line 1 (String.length line - 1)) in
  (dir, amount)

let read_moves filename =
  let ch = open_in filename in
  let rec loop acc =
    match input_line ch with
    | line -> loop (parse_line line :: acc)
    | exception End_of_file ->
      close_in ch;
      List.rev acc
  in
  loop []

let solve_puzzle1 moves =
  List.length moves

let solve_puzzle2 moves =
  List.length moves * 2
