let () =
  let use_sample =
    Array.length Sys.argv > 1 && Sys.argv.(1) = "--sample"
  in
  let filename = if use_sample then "day01/sample.txt" else "day01/input.txt" in
  let moves = Day01.Solver.read_moves filename in
  let ans = Day01.Solver.solve_puzzle1 moves in
  Printf.printf "%d\n" ans
