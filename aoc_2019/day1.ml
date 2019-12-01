open Core

let run () =
  let test = Util.read_lines_to_string "dune-project" in
  print_string (test ^ "\n")
