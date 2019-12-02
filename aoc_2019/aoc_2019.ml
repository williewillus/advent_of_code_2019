open Core

let main () =
  let d = Array.get Sys.argv 1 in
  match (int_of_string d) with
    1 -> Day1.run ()
  | 2 -> Day2.run ()
  | _ -> invalid_arg "Unknown day"

let () = main ()
