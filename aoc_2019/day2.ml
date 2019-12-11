open Core

let simulate data in1 in2 =
  let state = Intcode.State.init data in
  let () = Intcode.State.set state 1 in1 in
  let () = Intcode.State.set state 2 in2 in
  let exit = ref false in
  while not !exit do
    exit := Intcode.State.dispatch state
  done;
  Intcode.State.get state 0

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
