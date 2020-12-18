let compute x = (x / 3) - 2

let compute_p2 x =
  let rec compute_fp acc x =
    (* overshot and added x into acc when we shouldn'tve, so back up one
       this could also be checked in the else, but whatever *)
    if x <= 0 then acc - x 
    else
      let c = compute x in
      compute_fp (acc + c) c
  in
  compute_fp 0 x
    
let run () =
  let lines = Util.read_all_lines "d1_input.txt"
              |> List.map int_of_string in 
  let p1 = List.map compute lines
           |> List.fold_left (+) 0 in
  let p2 = List.map compute_p2 lines
           |> List.fold_left (+) 0 in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2;

