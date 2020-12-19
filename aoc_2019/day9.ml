let run () =
  let input = Util.read_lines_to_string "d9_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
  let () = print_string "For part 1, input '1'; for part 2, input '2';\n" in
  let state = Intcode.State.init data in
  Intcode.State.dispatch_all state
