open Core
module Dir = Util.Dir
module Point = Util.Point           

type color =
  | White
  | Black
  | Uncolored

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

  let dump state =
    let f k =
      let white = Hashtbl.find_exn state k = 1 in
      if white then
        Printf.printf "%d\t%d\n" k.Point.x k.Point.y
    in
    Hashtbl.iter_keys ~f state
end
                  
let simulate data p2 =
  let robot_state = RobotState.init () in
  if p2 then
    RobotState.set robot_state 1;
  let inh = RobotState.input_handler robot_state in
  let ouh = RobotState.output_handler robot_state in
  let state = Intcode.State.init ~input_handler:inh ~output_handler:ouh data in
  let exit = ref false in
  while not !exit do
    exit := Intcode.State.dispatch state
  done;
  robot_state

let run () =
  let input = Util.read_lines_to_string "d11_input.txt" in
  let data = String.split input ~on:','
             |> List.map ~f:int_of_string in
  let p1 = simulate data false in
  let () = Printf.printf "Part 1: %d\n" !(p1.painted) in
  let p2 = simulate data true in
  let () = RobotState.dump p2.board in
  Printf.printf "Part 2: Plot ^ with your choice of plotter (I'm lazy)\n"

