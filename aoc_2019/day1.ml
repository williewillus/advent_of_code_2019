open Core

let compute x = (x / 3) - 2

let compute_p2 x =
  let rec compute_fp x =
    if x <= 0 then 0
    else x + compute_fp (compute x) (* TODO: Make this properly TCO-able? *)
  in
  compute_fp x - x (* Remove the initial mass *)
    
let run () =
  let lines = Util.read_all_lines "d1_input.txt"
              |> List.map ~f:int_of_string in 
  let p1 = List.map lines ~f:compute
          |> List.fold ~init:0 ~f:(+) in
  let p2 = List.map lines ~f:compute_p2
           |> List.fold ~init:0 ~f:(+) in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2;

