type vec2 = {
    x : float;
    y : float;
  }

let mag {x; y} = sqrt (x *. x +. y *. y)

let normalize v =
  let m = mag v in
  {x = v.x /. m; y = v.y /. m}

let dist v1 v2 = mag {x = v2.x -. v1.x; y = v2.y -. v1.y}

let eps_eq v1 v2 =
  let d = abs_float (dist v1 v2) in
  d < epsilon_float

let asteroids_visible_from grid sx sy =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let seen = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if (y <> sy) && (x <> sx) && grid.(y).(x) = '#' then
        let delta = {x = float_of_int (x - sx); y = float_of_int (y - sy)}
                    |> normalize in
        if not (Base.List.mem !seen delta ~equal:eps_eq) then
          seen := delta::!seen
    done
  done;
  Printf.printf "Seen directions from %d, %d: " sx sy;
  List.iter (fun v -> Printf.printf "[%.4f, %.4f]" v.x v.y) !seen;
  print_newline ();
  List.length !seen

let run () =
  let lines = Util.read_all_lines "d10_input.txt" in
  let grid = List.map
               (fun l -> Array.of_seq (String.to_seq l))
               lines
             |> Array.of_list in
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  let max_seen = ref (-1) in
  let max_seen_pos = ref (-1, -1) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if grid.(y).(x) = '#' then
        let visible = asteroids_visible_from grid x y in
        let () = Printf.printf "%d, %d can see %d\n" x y visible in
        if visible > !max_seen then
          begin
            max_seen := visible;
            max_seen_pos := (x, y);
          end
    done
  done;
  Printf.printf "Part 1: %d at %d, %d\n" !max_seen (fst !max_seen_pos) (snd !max_seen_pos)
