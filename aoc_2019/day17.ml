let show grid =
  List.iter
    (fun row ->
      List.iter (fun ch -> print_char ch) row;
      print_newline ())
    grid

let run () =
  let input = Util.read_lines_to_string "d17_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
  let cur_row = ref [] in
  let grid = ref [] in
  let output_handler i =
    let ch = Char.chr i in
    match ch with
    | '\n' ->
       if !cur_row <> [] then begin
           grid := (List.rev !cur_row)::!grid;
           cur_row := [];
       end
    | '#' | '.' | '^' | 'v' | '<' | '>' ->
       cur_row := ch::!cur_row;
    | _ -> raise (Invalid_argument ("Unexpected output value: " ^ (string_of_int i)))
  in
  let state = Intcode.State.init data ~output_handler in
  let () = Intcode.State.dispatch_all state in
  let grid = List.rev !grid in
  let height = List.length grid in
  let width = List.length (List.hd grid) in
  let p1 = ref 0 in
  show grid;
  (* an intersection cannot happen on the outer edge *)
  for y = 1 to height-2 do
    for x = 1 to width-2 do
      let left = List.nth (List.nth grid y) (x - 1) in
      let right = List.nth (List.nth grid y) (x + 1) in
      let up = List.nth (List.nth grid (y - 1)) x in
      let down = List.nth (List.nth grid (y + 1)) x in
      let center = List.nth (List.nth grid y) x in
      if [left; right; up; down; center] = ['#'; '#'; '#'; '#'; '#'] then
        p1 := !p1 + (x * y)
    done
  done;
  Printf.printf "Part 1: %d\n" !p1
