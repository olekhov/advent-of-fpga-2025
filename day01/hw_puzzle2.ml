open Hardcaml

module HW = Day01.Safe_dial
module Sim = Cyclesim.With_interface (HW.I) (HW.O)

open! HW.I
open! HW.O

let run moves =
  let sim = Sim.create (HW.create (Scope.create ())) in
  let inputs : Bits.t ref HW.I.t = Cyclesim.inputs sim in
  (* reset *)
  inputs.rst := Bits.vdd;
  inputs.step := Bits.gnd;
  inputs.dir := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.rst := Bits.gnd;

  let apply_move (dir_chr, amount) =
    let dir =
      match dir_chr with
      | 'L' | 'l' -> false
      | 'R' | 'r' -> true
      | c -> invalid_arg (Printf.sprintf "unknown direction %c" c)
    in
    for _ = 1 to amount do
      inputs.step := Bits.vdd;
      inputs.dir := if dir then Bits.vdd else Bits.gnd;
      Cyclesim.cycle sim;
      inputs.step := Bits.gnd
    done
  in

  List.iter apply_move moves;
  Cyclesim.outputs ~clock_edge:After sim

let () =
  let use_sample =
    Array.length Sys.argv > 1 && Sys.argv.(1) = "--sample"
  in
  let filename = if use_sample then "day01/sample.txt" else "day01/input.txt" in
  let moves = Day01.Solver.read_moves filename in

  (* Count every crossing of 0 during the whole sequence. The hardware
     register zero_count already accumulates that. *)
  let o = run moves in
  let zero_count = Bits.to_int !(o.zero_count) in
  Printf.printf "%d\n%!" zero_count
