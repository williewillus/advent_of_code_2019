open Core

let main () =
  let d = Sys.argv.(1) in
  match (int_of_string d) with
    1 -> Day1.run ()
  | 2 -> Day2.run ()
  | 3 -> Day3.run ()
  | 4 -> Day4.run ()
  | 5 -> Day5.run ()
  | 6 -> Day6.run ()
  | 8 -> Day8.run ()
  | _ -> invalid_arg "Unknown day"

let () = main ()
