open Core

let simulate data =
  let state = Intcode.State.init data in
  let exit = ref false in
  while not !exit do
    exit := Intcode.State.dispatch state;
  done

let run () =
  let input = Util.read_lines_to_string "d9_input.txt" in
  let data = String.split input ~on:','
             |> List.map ~f:int_of_string in
  let () = print_string "For part 1, input '1'; for part 2, input '2';\n" in
  simulate data
