open Hardcaml

module HW = Day01.Safe_dial
module Sim = Cyclesim.With_interface (HW.I) (HW.O)

open! HW.I
open! HW.O

let string_of_move (dir, amt) =
  Printf.sprintf "%c%d" dir amt

let () =
  let use_sample =
    Array.length Sys.argv > 1 && Sys.argv.(1) = "--sample"
  in
  let filename = if use_sample then "day01/sample.txt" else "day01/input.txt" in
  let moves = Day01.Solver.read_moves filename in

  (* Run moves while also printing position after each move. *)
  let sim = Sim.create (HW.create (Scope.create ())) in
  let inputs : Bits.t ref HW.I.t = Cyclesim.inputs sim in
  (* reset *)
  inputs.rst := Bits.vdd;
  inputs.step := Bits.gnd;
  inputs.dir := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.rst := Bits.gnd;

  let moves_ending_at_zero = ref 0 in

  let print_state move =
    let o = Cyclesim.outputs ~clock_edge:After sim in
    let pos = Bits.to_int !(o.pos) in
    if pos = 0 then incr moves_ending_at_zero;
    Printf.printf "after %s -> pos=%d\n%!" (string_of_move move) pos
  in

  List.iter
    (fun m ->
      let dir =
        match m with
        | ('L', _) | ('l', _) -> false
        | ('R', _) | ('r', _) -> true
        | c, _ -> invalid_arg (Printf.sprintf "unknown direction %c" c)
      in
      let _, amount = m in
      for _ = 1 to amount do
        inputs.step := Bits.vdd;
        inputs.dir := if dir then Bits.vdd else Bits.gnd;
        Cyclesim.cycle sim;
        inputs.step := Bits.gnd
      done;
      print_state m)
    moves;

  Printf.printf "moves-ending-at-zero=%d\n%!" !moves_ending_at_zero
