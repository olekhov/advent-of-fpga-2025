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


let rotate pos dir amount =
  match dir with
  | 'L' -> if pos - amount <  0 then pos + 100 - amount else pos - amount
  | 'R' -> if pos + amount > 99 then pos - 100 + amount else pos + amount
  | _ -> pos

let solve_puzzle1 moves =
  let _, count_zero = List.fold_left
    (fun (pos, count) (dir, amount) ->
      let pos' = rotate pos dir amount in
      (pos', if pos' = 0 then count + 1 else count)
    )
    (50, 0)
    moves
  in
  count_zero
  

let clicking_rotate pos dir amount =

  let step (pos, count) _ =
    let pos' =
      match dir with
      | 'L' -> if pos =  0 then 99 else pos - 1
      | 'R' -> if pos = 99 then  0 else pos + 1
      | _ -> pos
    in
    let count' = if pos' = 0 then count + 1 else count in
    (pos', count')
  in

  let final_pos, clicks =
    Seq.(ints 0 |> take amount |> fold_left step (pos, 0))
  in

  (final_pos, clicks)


let solve_puzzle2 moves =
  let _, count_zero = List.fold_left
    (fun (pos, count) (dir, amount) ->
      let (pos', count') = clicking_rotate pos dir amount in
      (pos', count + count')
    )
    (50, 0)
    moves
  in
  count_zero
