let simulate data in1 in2 =
  let state = Intcode.State.init data in
  let () = Intcode.State.set state 1 in1 in
  let () = Intcode.State.set state 2 in2 in
  let () = Intcode.State.dispatch_all state in
  Intcode.State.get state 0

let run () =
  let input = Util.read_lines_to_string "d2_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
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
          ()
      end
    done
  done
