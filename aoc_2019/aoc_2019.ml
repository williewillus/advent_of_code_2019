let main () =
  let d = Sys.argv.(1) in
  let start_time = Unix.gettimeofday () in
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
    | 10 -> Day10.run ()
    | 11 -> Day11.run ()
    | 12 -> Day12.run ()
    | 13 -> Day13.run ()
    | 16 -> Day16.run ()
    | _ -> invalid_arg "Unknown day"
  in
  let end_time = Unix.gettimeofday () in
  Printf.printf "Completed in %f s\n" (end_time -. start_time)

let () = main ()
