open Core

let main () =
  let d = (Sys.get_argv ()).(1) in
  let start_time = Time_ns.now () in
  let () =
    match (int_of_string d) with
    | 1 -> Day1.run ()
    | 2 -> Day2.run ()
    | 3 -> Day3.run ()
    | 4 -> Day4.run ()
    | 5 -> Day5.run ()
    | 6 -> Day6.run ()
    | 7 -> Day7.run ()
    | 8 -> Day8.run ()
    | 9 -> Day9.run ()
    | _ -> invalid_arg "Unknown day"
  in
  let end_time = Time_ns.now () in
  Printf.printf "Completed in %d ms\n" (Time_ns.Span.to_int_ms (Time_ns.diff end_time start_time);)

let () = main ()
