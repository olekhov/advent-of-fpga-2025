open! Hardcaml
open! Signal

module I = struct
  type 'a t = {
    clk   : 'a;
    rst   : 'a;
    step  : 'a;        (* 1 = one click *)
    dir   : 'a;        (* 0 = L, 1 = R *)
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    pos        : 'a[@bits 7];    (* 0..99 *)
    zero_pulse : 'a;             (* 1 when pos is 0 *)
    zero_count : 'a[@bits 32];
  } [@@deriving hardcaml]
end

let step_pos pos dir =
  let open Signal in
  let is_left = ~: dir in   (* dir=0 -> L, dir=1 -> R *)
  let at_zero = pos ==:. 0 in
  let at_99   = pos ==:. 99 in
  mux2 is_left
    (mux2 at_zero (of_int ~width:7 99) (pos -:. 1))
    (mux2 at_99   (of_int ~width:7 0)  (pos +:. 1))


(*****************************)
let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let open Signal in

  let pos_init = of_int ~width:7 50 in

  let pos_spec =
    Reg_spec.create ~clock:i.clk ~reset:i.rst ()
    |> Reg_spec.override ~reset_to:pos_init
  in

  let pos = reg_fb
    pos_spec
    ~width:7
    ~f:(fun prev ->
      mux2 i.step (step_pos prev i.dir) prev
    )
  in

  let zero_count_spec =
    Reg_spec.create ~clock:i.clk ~clear:i.rst ()
    |> Reg_spec.override ~clear_to:(zero 32)
  in

  let zero_count = reg_fb
    zero_count_spec
    ~width:32
    ~f:(fun prev ->
      let next_pos = step_pos pos i.dir in
      let hit_zero = i.step &: (next_pos ==:. 0) in
      mux2 hit_zero (prev +:. 1) prev
    )
  in

  let zero_pulse = i.step &: (pos ==:. 0) in

  { O.
    pos;
    zero_pulse;
    zero_count;
  }
