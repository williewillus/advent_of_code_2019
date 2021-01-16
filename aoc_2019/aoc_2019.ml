let days = [|
    Some Day1.run; Some Day2.run; Some Day3.run; Some Day4.run; Some Day5.run;
    Some Day6.run; Some Day7.run; Some Day8.run; Some Day9.run; None;
    Some Day11.run; Some Day12.run; Some Day13.run; None; None;
    Some Day16.run; Some Day17.run; None; Some Day19.run; None;
    None; None; Some Day23.run; Some Day24.run; None;
  |]

let exclude_from_all = [| 23 |]

let main () =
  let d = Sys.argv.(1) in
  let start_time = Unix.gettimeofday () in
  let () = 
    if d = "all" then
      let f i = function
        | Some day when not (Array.mem (i+1) exclude_from_all) ->
           let () = Printf.printf "== Running day %d ==\n" (i + 1) in
           day ()
        | _ -> ()
      in
      Array.iteri f days
    else
      let day = int_of_string d in
      let () = Printf.printf "== Running day %d ==\n" day in
      match days.(day - 1) with
      | Some day -> day ()
      | None -> raise (Invalid_argument "Invalid day")
  in
  let end_time = Unix.gettimeofday () in
  Printf.eprintf "Completed in %f s\n" (end_time -. start_time)

let () = main ()
