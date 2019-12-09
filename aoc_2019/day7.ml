open Core

module State = struct
  type t = {
      mem : int Array.t;
      pc : int;
      input : int option;
      output : int option;
    }

  (* Note: i is 1-indexed *)
  let read_param state i =
    let insn = state.mem.(state.pc) in
    let div = Int.pow 10 (i + 1) in
    let param_mode = (insn / div) % 10 in
    match param_mode with
    | 0 -> state.mem.(state.mem.(state.pc + i))
    | 1 -> state.mem.(state.pc + i)
    | _ -> invalid_arg "Unknown parameter mode"

  let dispatch state =
    match (state.mem.(state.pc) % 100) with
    | 1 -> begin
        let l = read_param state 1 in 
        let r = read_param state 2 in 
        let dest = state.mem.(state.pc + 3) in
        state.mem.(dest) <- l + r;
        Some (state.pc + 4)
      end
    | 2 -> begin
        let l = read_param state 1 in 
        let r = read_param state 2 in 
        let dest = state.mem.(state.pc + 3) in
        state.mem.(dest) <- l * r;
        Some (state.pc + 4)
      end
    | 3 ->
       let () = Out_channel.flush Out_channel.stdout in
       let v = int_of_string (In_channel.input_line_exn In_channel.stdin) in
       let dest = state.mem.(state.pc + 1) in
       state.mem.(dest) <- v;
       Some (state.pc + 2)
    | 4 ->
       let v = read_param state 1 in
       Printf.printf "%d\n" v;
       Some (state.pc + 2)
    | 5 ->
       let tst = read_param state 1 in
       let dest = read_param state 2 in
       if tst <> 0 then
         Some dest
       else Some (state.pc + 3)
    | 6 ->
       let tst = read_param state 1 in
       let dest = read_param state 2 in
       if tst = 0 then
         Some dest
       else Some (state.pc + 3)
    | 7 ->
       let a = read_param state 1 in
       let b = read_param state 2 in
       let dest = state.mem.(state.pc + 3) in
       state.mem.(dest) <- if a < b then 1 else 0;
       Some (state.pc + 4)
    | 8 ->
       let a = read_param state 1 in
       let b = read_param state 2 in
       let dest = state.mem.(state.pc + 3) in
       state.mem.(dest) <- if a = b then 1 else 0;
       Some (state.pc + 4)
    | 99 -> None
    | _ as i -> invalid_arg ("Unknown opcode" ^ (string_of_int i))

end

let run () = () 
