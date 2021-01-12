type status =
  | Stationary
  | Pulled

let run () =
  let input = Util.read_lines_to_string "d19_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
  let p1 = ref 0 in
  for x = 0 to 49 do
    for y = 0 to 49 do
      let cur_input = ref [x; y] in
      let cur_input_idx = ref 0 in
      let input_handler () =
        let ret = List.nth !cur_input !cur_input_idx in
        if !cur_input_idx = 0 then
          cur_input_idx := 1
        else
          cur_input_idx := 0;
        ret
      in    
      let cur_output = ref None in
      let output_handler = function
        | 0 -> cur_output := Some Stationary
        | 1 -> cur_output := Some Pulled
        | i -> raise (Invalid_argument (Printf.sprintf "Unexpected output value %d\n" i))
      in
      let state = Intcode.State.init ~input_handler ~output_handler data in
      let _: Intcode.State.dispatch_result = Intcode.State.dispatch_until state (fun () -> Option.is_some !cur_output) in
      let () = match Option.get !cur_output with
        | Stationary -> ()
        | Pulled -> p1 := !p1 + 1
      in
      cur_output := None
    done
  done;
  Printf.printf "Part 1: %d\n" !p1
