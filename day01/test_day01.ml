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
  Cyclesim.cycle sim;
  i.rst := Bits.gnd

let step_once sim i ~dir =
  i.step := Bits.vdd;
  i.dir := bit dir;
  Cyclesim.cycle sim;
  i.step := Bits.gnd

let outputs_after sim : Bits.t ref HW.O.t =
  Cyclesim.outputs ~clock_edge:After sim

let outputs_before sim : Bits.t ref HW.O.t =
  Cyclesim.outputs ~clock_edge:Before sim

let with_fresh_sim f =
  let sim = Sim.create (HW.create (Scope.create ())) in
  let inputs : Bits.t ref HW.I.t = Cyclesim.inputs sim in
  reset sim inputs;
  f sim inputs

let () =
  (* One click left *)
  with_fresh_sim (fun sim i ->
    step_once sim i ~dir:false;
    let o = outputs_after sim in
    assert (Bits.to_int !(o.pos) = 49);
    assert (Bits.is_gnd !(o.zero_pulse)));

  (* One click right *)
  with_fresh_sim (fun sim i ->
    step_once sim i ~dir:true;
    let o = outputs_after sim in
    assert (Bits.to_int !(o.pos) = 51);
    assert (Bits.is_gnd !(o.zero_pulse)));

  (* Full turn left (100 steps) returns to start, zero_count increments once *)
  with_fresh_sim (fun sim i ->
    for _ = 1 to 100 do
      step_once sim i ~dir:false
    done;
    let o = outputs_after sim in
    let count = Bits.to_int !(o.zero_count) in
    if count <> 1 then failwith (Printf.sprintf "zero_count (left) = %d" count);
    assert (Bits.to_int !(o.pos) = 50));

  (* Full turn right (100 steps) returns to start, zero_count increments once *)
  with_fresh_sim (fun sim i ->
    for _ = 1 to 100 do
      step_once sim i ~dir:true
    done;
    let o = outputs_after sim in
    let count = Bits.to_int !(o.zero_count) in
    if count <> 1 then failwith (Printf.sprintf "zero_count (right) = %d" count);
    assert (Bits.to_int !(o.pos) = 50));

  (* zero_pulse asserted when stepping while at zero *)
  with_fresh_sim (fun sim i ->
    (* Drive left until position hits zero (max 120 steps for safety). *)
    let rec drive_to_zero attempts_left =
      if attempts_left = 0 then failwith "did not reach zero position"
      else
        let o = outputs_after sim in
        if Bits.to_int !(o.pos) = 0 then ()
        else (step_once sim i ~dir:false; drive_to_zero (attempts_left - 1))
    in
    drive_to_zero 120;

    (* Prime a step from 0 to check zero_pulse before the clock edge. *)
    i.step := Bits.vdd;
    i.dir := bit false;
    Cyclesim.cycle_check sim;
    Cyclesim.cycle_before_clock_edge sim;
    let o_before = outputs_before sim in
    assert (Bits.is_vdd !(o_before.zero_pulse));
    (* Complete the edge to keep sim consistent. *)
    i.clk := Bits.vdd;
    Cyclesim.cycle_at_clock_edge sim;
    Cyclesim.cycle_after_clock_edge sim;
    i.step := Bits.gnd)
