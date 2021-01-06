let run () =
  let input = Util.read_lines_to_string "d9_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
  let state_p1 = Intcode.State.init data ~input_handler:(fun _ -> 1) in
  let () = Intcode.State.dispatch_all state_p1 in
  let state_p2 = Intcode.State.init data ~input_handler:(fun _ -> 2) in
  Intcode.State.dispatch_all state_p2
