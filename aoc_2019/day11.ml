module Dir = Util.Dir
module Hash_set = Base.Hash_set
module Hashtbl = Base.Hashtbl
module Point = Util.Point           

(* Dirty and imperative, but whatever *)
module RobotState = struct
  type want_state =
    | Paint
    | Turn

  type t =
    {
      pos : Point.t ref;
      facing : Dir.t ref;
      board : (Point.t, int) Hashtbl.t;
      expected_output : want_state ref;
      painted : int ref;
    }

  let init () =
    {
      pos = ref {Point.x = 0; Point.y = 0};
      facing = ref Dir.Up;
      board = Hashtbl.create (module Point);
      expected_output = ref Paint;
      painted = ref 0;
    }

  let get state =
    match Hashtbl.find state.board !(state.pos) with
    | None -> 0
    | Some i -> i

  let set state v = Hashtbl.set state.board ~key:!(state.pos) ~data:v

  let input_handler state () = get state

  let output_handler state v =
    match !(state.expected_output) with
    | Paint ->
       let () = state.expected_output := Turn in
       let already_painted = Option.is_some (Hashtbl.find state.board !(state.pos)) in
       let () = if not already_painted then state.painted := !(state.painted) + 1 in
       set state v
    | Turn ->
       let () = state.expected_output := Paint in
       let () = match v with
         | 0 -> state.facing := Util.Dir.rot_left !(state.facing)
         | 1 -> state.facing := Util.Dir.rot_right !(state.facing)
         | _ -> failwith "Unknown turn command" in
       state.pos := Util.Dir.move !(state.pos) !(state.facing)

  let whites state =
    Hashtbl.to_alist state
    |> List.filter_map (fun (k, v) -> if v = 1 then Some k else None)
end
                  
let simulate data p2 =
  let robot_state = RobotState.init () in
  if p2 then
    RobotState.set robot_state 1;
  let inh = RobotState.input_handler robot_state in
  let ouh = RobotState.output_handler robot_state in
  let state = Intcode.State.init ~input_handler:inh ~output_handler:ouh data in
  let () = Intcode.State.dispatch_all state in
  robot_state


let run () =
  let input = Util.read_lines_to_string "d11_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
  let p1 = simulate data false in
  let () = Printf.printf "Part 1: %d\n" !(p1.painted) in

  let p2 = simulate data true in
  let min_x_pos ps = List.fold_left (fun m v -> min m v.Point.x) Int.max_int ps in
  let min_y_pos ps = List.fold_left (fun m v -> min m v.Point.y) Int.max_int ps in
  let recenter poses =
    let min_x = min_x_pos poses in
    let min_y = min_y_pos poses in
    List.map (fun p -> { Point.x = p.Point.x - min_x; y = p.Point.y - min_y}) poses
  in
  let whites = recenter (RobotState.whites p2.board) in
  let min_x = min_x_pos whites in
  let min_y = min_y_pos whites in
  let max_x = List.fold_left (fun m v -> max m v.Point.x) Int.min_int whites in
  let max_y = List.fold_left (fun m v -> max m v.Point.y) Int.min_int whites in
  let set = Hash_set.of_list (module Point) whites in
  let () = print_endline "Part 2:" in
  for y = max_y downto min_y do
    for x = min_x to max_x do
      let p = { Point.x = x; y = y } in
      if Hash_set.mem set p then
        print_string "\u{2588}"
      else
        print_string "\u{2591}"
    done;
    print_newline ()
  done

