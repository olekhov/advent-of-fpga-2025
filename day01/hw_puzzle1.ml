open Hardcaml

module HW = Day01.Safe_dial
module Sim = Cyclesim.With_interface (HW.I) (HW.O)

open! HW.I
open! HW.O

let bit b = if b then Bits.vdd else Bits.gnd

let reset sim i =
  i.rst := Bits.vdd;
  i.step := Bits.gnd;
  i.dir := Bits.gnd;
  i.clk := Bits.vdd;
  Cyclesim.cycle sim;
  i.rst := Bits.gnd

let tick sim i ~step ~dir =
  i.step := bit step;
  i.dir := bit dir;
  Cyclesim.cycle sim;
  i.step := Bits.gnd

let repeat n f =
  let rec loop k = if k <= 0 then () else (f (); loop (k - 1)) in
  loop n

let apply_move sim i (dir_chr, amount) =
  let dir =
    match dir_chr with
    | 'L' | 'l' -> false
    | 'R' | 'r' -> true
    | c -> invalid_arg (Printf.sprintf "unknown direction %c" c)
  in
  repeat amount (fun () -> tick sim i ~step:true ~dir)

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
  reset sim inputs;

  let moves_ending_at_zero = ref 0 in

  let print_state move =
    let o = Cyclesim.outputs ~clock_edge:After sim in
    let pos = Bits.to_int !(o.pos) in
    if pos = 0 then incr moves_ending_at_zero;
    Printf.printf "after %s -> pos=%d\n%!" (string_of_move move) pos
  in

  List.iter
    (fun m ->
      apply_move sim inputs m;
      print_state m)
    moves;

  Printf.printf "moves-ending-at-zero=%d\n%!" !moves_ending_at_zero
