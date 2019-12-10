open Core

module State = struct
  type t =
    {
      mem : (int, int) Hashtbl.t;
      pc : int ref;
      relbase : int ref;
    }

  let get state addr =
    Hashtbl.find state.mem addr |> Option.value ~default:0

  let set state addr v =
    Hashtbl.set state.mem ~key:addr ~data:v

  let init image =
    let mem = Hashtbl.create (module Int) in
    let () = List.iteri image ~f:(fun addr v -> Hashtbl.set mem ~key:addr ~data:v) in
    {
      mem;
      pc = ref 0;
      relbase = ref 0;
    }

  (* Note: i is 1-indexed *)
  let resolve_param state i =
    let insn = get state !(state.pc) in
    let param = get state (!(state.pc) + i) in
    let div = Int.pow 10 (i + 1) in
    let param_mode = (insn / div) % 10 in
    match param_mode with
    | 0 -> param 
    | 1 -> invalid_arg "Cannot write to an immediate"
    | 2 -> !(state.relbase) + param
    | _ -> invalid_arg "Unknown parameter mode"

  let read_param state i =
    let insn = get state (!(state.pc)) in
    let param = get state (!(state.pc) + i) in
    let div = Int.pow 10 (i + 1) in
    let param_mode = (insn / div) % 10 in
    match param_mode with
    | 0 -> get state param
    | 1 -> param
    | 2 -> get state (!(state.relbase) + param)
    | _ -> invalid_arg "Unknown parameter mode"

  let dispatch state =
    match (get state !(state.pc) % 100) with
    | 1 -> begin
        let l = read_param state 1 in 
        let r = read_param state 2 in 
        set state (resolve_param state 3) (l + r);
        state.pc := !(state.pc) + 4;
        false
      end
    | 2 -> begin
        let l = read_param state 1 in 
        let r = read_param state 2 in 
        set state (resolve_param state 3) (l * r);
        state.pc := !(state.pc) + 4;
        false
      end
    | 3 ->
       let () = Out_channel.flush Out_channel.stdout in
       let v = int_of_string (In_channel.input_line_exn In_channel.stdin) in
       set state (resolve_param state 1) v;
       state.pc := !(state.pc) + 2;
       false
    | 4 ->
       let v = read_param state 1 in
       Printf.printf "%d\n" v;
       state.pc := !(state.pc) + 2;
       false
    | 5 ->
       let tst = read_param state 1 in
       let dest = read_param state 2 in
       if tst <> 0 then
         state.pc := dest
       else
         state.pc := !(state.pc) + 3;
       false
    | 6 ->
       let tst = read_param state 1 in
       let dest = read_param state 2 in
       if tst = 0 then
         state.pc := dest
       else
         state.pc := !(state.pc) + 3;
       false
    | 7 ->
       let a = read_param state 1 in
       let b = read_param state 2 in
       set state (resolve_param state 3) (if a < b then 1 else 0);
       state.pc := !(state.pc) + 4;
       false
    | 8 ->
       let a = read_param state 1 in
       let b = read_param state 2 in
       set state (resolve_param state 3) (if a = b then 1 else 0);
       state.pc := !(state.pc) + 4;
       false
    | 9 ->
       let v = read_param state 1 in
       state.relbase := !(state.relbase) + v;
       state.pc := !(state.pc) + 2;
       false
    | 99 -> true
    | _ as i -> invalid_arg ("Unknown opcode" ^ (string_of_int i))
end

let simulate data =
  let state = State.init data in
  let exit = ref false in
  while not !exit do
    exit := State.dispatch state;
  done

let run () =
  let input = Util.read_lines_to_string "d9_input.txt" in
  let data = String.split input ~on:','
             |> List.map ~f:int_of_string in
  let () = print_string "For part 1, input '1'; for part 2, input '2';\n" in
  simulate data
