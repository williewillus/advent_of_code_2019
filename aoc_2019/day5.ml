open Core

(* Note: i is 1-indexed *)
let read_param arr pc i =
  let insn = arr.(pc) in
  let div = Int.pow 10 (i + 1) in
  let param_mode = (insn / div) % 10 in
  match param_mode with
  | 0 -> arr.(arr.(pc + i))
  | 1 -> arr.(pc + i)
  | _ -> invalid_arg "Unknown parameter mode"

let dispatch arr pc =
  match (arr.(pc) % 100) with
  | 1 -> begin
      let l = read_param arr pc 1 in 
      let r = read_param arr pc 2 in 
      let dest = arr.(pc + 3) in
      arr.(dest) <- l + r;
      Some (pc + 4)
    end
  | 2 -> begin
      let l = read_param arr pc 1 in 
      let r = read_param arr pc 2 in 
      let dest = arr.(pc + 3) in
      arr.(dest) <- l * r;
      Some (pc + 4)
    end
  | 3 ->
     let () = Out_channel.flush Out_channel.stdout in
     let v = int_of_string (In_channel.input_line_exn In_channel.stdin) in
     let dest = arr.(pc + 1) in
     arr.(dest) <- v;
     Some (pc + 2)
  | 4 ->
     let v = read_param arr pc 1 in
     Printf.printf "%d\n" v;
     Some (pc + 2)
  | 5 ->
     let tst = read_param arr pc 1 in
     let dest = read_param arr pc 2 in
     if tst <> 0 then
       Some dest
     else Some (pc + 3)
  | 6 ->
     let tst = read_param arr pc 1 in
     let dest = read_param arr pc 2 in
     if tst = 0 then
       Some dest
     else Some (pc + 3)
  | 7 ->
     let a = read_param arr pc 1 in
     let b = read_param arr pc 2 in
     let dest = arr.(pc + 3) in
     arr.(dest) <- if a < b then 1 else 0;
     Some (pc + 4)
  | 8 ->
     let a = read_param arr pc 1 in
     let b = read_param arr pc 2 in
     let dest = arr.(pc + 3) in
     arr.(dest) <- if a = b then 1 else 0;
     Some (pc + 4)
  | 99 -> None
  | _ as i -> invalid_arg ("Unknown opcode" ^ (string_of_int i))

let simulate data =
  let data = Array.of_list data in
  let pc = ref 0 in
  let exit = ref false in
  while not !exit do
    match dispatch data !pc with
    | Some new_pc -> pc := new_pc
    | None -> exit := true
  done;
  ()

let run () =
  let input = Util.read_lines_to_string "d5_input.txt" in
  let data = String.split input ~on:','
             |> List.map ~f:int_of_string in
  let () = print_string "For part 1, input '1'; for part 2, input '5';\n" in
  simulate data;
