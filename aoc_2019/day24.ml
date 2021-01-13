let dump grid =
  let f row =
    let s = Base.String.of_char_list (Array.to_list row) in
    print_endline s
  in
  Array.iter f grid;
  print_newline ()

let biodiversity grid =
  let proc_row acc row =
    Array.fold_left
      (fun (cur_value, cur_score) ch ->
        if ch = '#' then
          (cur_value + 1, cur_score + (1 lsl cur_value))
        else
          (cur_value + 1, cur_score))
      acc row
  in
  let (_, score) = Array.fold_left proc_row (0, 0) grid
  in
  score

let iter grid =
  let height = Array.length grid in
  let next = Array.copy grid in
  for i = 0 to height - 1 do
    (* deep copy each row *)
    next.(i) <- Array.copy next.(i)
  done;

  for i = 0 to height - 1 do
    let width = Array.length grid.(i) in
    for j = 0 to width - 1 do
      let ch = grid.(i).(j) in
      let up_bug = if i = 0 then false else grid.(i-1).(j) = '#' in
      let down_bug = if i = height-1 then false else grid.(i+1).(j) = '#' in
      let left_bug = if j = 0 then false else grid.(i).(j-1) = '#' in
      let right_bug = if j = width-1 then false else grid.(i).(j+1) = '#' in
      let num_bug_nbs = [up_bug; down_bug; left_bug; right_bug]
                        |> List.filter (fun x -> x)
                        |> List.length in
      if ch = '.' && (num_bug_nbs = 1 || num_bug_nbs = 2) then (
        next.(i).(j) <- '#'
      );
      
      if ch = '#' && num_bug_nbs <> 1 then (
        next.(i).(j) <- '.';
      );
    done
  done;
  next

let run () =
  let seen = Hashtbl.create 0 in
  let lines = Util.read_all_lines "d24_input.txt" in
  let grid = List.map Base.String.to_array lines
             |> Array.of_list in
  let rec p1 state =
    if Hashtbl.mem seen state then
      biodiversity state
    else
      let () = Hashtbl.add seen state () in
      p1 (iter state)
  in
  Printf.printf "Part 1: %d\n" (p1 grid);
  ()
