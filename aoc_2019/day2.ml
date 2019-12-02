open Core

let dispatch arr pc =
  match Array.get arr pc with
    1 -> begin
      let l = Array.get arr (Array.get arr (pc + 1)) in 
      let r = Array.get arr (Array.get arr (pc + 2)) in 
      let dest = Array.get arr (pc + 3) in
      Array.set arr dest (l + r);
      false
    end
  | 2 -> begin
      let l = Array.get arr (Array.get arr (pc + 1)) in 
      let r = Array.get arr (Array.get arr (pc + 2)) in 
      let dest = Array.get arr (pc + 3) in
      Array.set arr dest (l * r);
      false
    end
  | 99 -> true
  | _ as i -> invalid_arg ("Unknown opcode" ^ (string_of_int i))

let simulate data in1 in2 =
  let data = Array.of_list data in
  Array.set data 1 in1;
  Array.set data 2 in2;
  let pc = ref 0 in
  let exit = ref false in
  while not !exit do
    exit := dispatch data !pc;
    pc := !pc + 4;
  done;
  Array.get data 0

let run () =
  let input = Util.read_lines_to_string "d2_input.txt" in
  let data = String.split input ~on:','
             |> List.map ~f:int_of_string in
  let p1 = simulate data 12 2 in
  Printf.printf "Part 1: %d\n" p1;

  (* Assume all inputs start with 1,x,x,x. Then we only have to brute force [0, data_len)
     since otherwise we'd access stuff out of bounds, which is undefined in the problem.
   *)
  let data_len = List.length data in
  for i = 0 to data_len - 1 do
    for j = 0 to data_len - 1 do
      let r = simulate data i j in
      if r = 19690720 then begin
          Printf.printf "Part 2: %d\n" (100 * i + j);
          exit 0
      end
    done
  done
