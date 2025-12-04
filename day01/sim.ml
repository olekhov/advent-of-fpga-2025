open Hardcaml
module HW = Day01.Safe_dial
module Sim = Cyclesim.With_interface (HW.I) (HW.O)

open! HW.I
open! HW.O

let reset sim i =
  i.rst := Bits.vdd;
  i.step := Bits.gnd;
  i.dir := Bits.gnd;
  Cyclesim.cycle sim;
  i.rst := Bits.gnd

let tick sim i ~step ~dir =
  i.step := if step then Bits.vdd else Bits.gnd;
  i.dir := if dir then Bits.vdd else Bits.gnd;
  Cyclesim.cycle sim

let print_outputs o =
  Printf.printf "pos=%d zero_cnt=%d zero_pulse=%b\n%!"
    (Bits.to_int !(o.pos))
    (Bits.to_int !(o.zero_count))
    (Bits.is_vdd !(o.zero_pulse))

let () =
  let sim = Sim.create (HW.create (Scope.create ())) in
  let (i : Bits.t ref HW.I.t) = Cyclesim.inputs sim in
  let (o : Bits.t ref HW.O.t) = Cyclesim.outputs ~clock_edge:After sim in
  reset sim i;
  print_endline "== after reset ==";
  print_outputs o;

  let step dir =
    tick sim i ~step:true ~dir;
    print_outputs o
  in

  print_endline "== stepping: L, L, R ==";
  step false;  (* left *)
  step false;  (* left *)
  step true;   (* right *)
