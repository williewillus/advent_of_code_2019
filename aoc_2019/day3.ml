open Core

type dir =
  | Up
  | Down
  | Left
  | Right

let vert d = d == Up || d == Down
let horz d = d == Left || d == Right
let perpendicular a b =
  match [a; b] with
  | [Up; Down] -> false
  | [Down; Up] -> false
  | [Left; Right] -> false
  | [Right; Left] -> false
  | _ -> true
                   
type point = {
    x : int;
    y : int;
  }

type move = (dir * int)
let do_move m p =
  match (fst m) with
  | Up -> {p with y = p.y + (snd m)}
  | Down -> {p with y = p.y - (snd m)}
  | Left -> {p with x = p.x - (snd m)}
  | Right -> {p with x = p.x + (snd m)}


let show_pt pt = Printf.sprintf "(%d, %d)" pt.x pt.y

type wire = point list

let show_wire w = List.map ~f:show_pt w
                |> String.concat ~sep:", "

let parse_wire (line : string) : wire =
  let parse_move acc move =
    let cur_pos = List.hd_exn acc in
    let direction = String.get move 0 in
    let amount = int_of_string (String.sub move ~pos:1 ~len:(String.length move - 1)) in
    let new_pos = match direction with
        'L' -> {cur_pos with x = cur_pos.x - amount}
      | 'R' -> {cur_pos with x = cur_pos.x + amount}
      | 'U' -> {cur_pos with y = cur_pos.y + amount}
      | 'D' -> {cur_pos with y = cur_pos.y - amount}
      |  _  -> invalid_arg "Unknown direction"
    in
    new_pos :: acc
  in
  String.split line ~on:','
  |> List.fold ~init:[{x = 0; y = 0;}] ~f:parse_move
  |> List.rev

(* Every wire is a list of moves (mag + dir)
   for each move (tracking current position), iterate through moves of every other wire
   intersection between m1, m2 if 
   1. directions are perpendicular AND
   2. m1 is vert and m2 is horz -> m2.y contained in m1.y-m1.y' and m2.x passes over m1.x
   3. vice versa 2
*)

(* when point p1 makes move m1, and point p2 makes move m2, does an intersection occur? 
   if so, returns the point of intersection, else none *)
let one_intersect (p1 : point) (m1 : move) (p2 : point) (m2 : move) : point option =
  if not (perpendicular (fst m1) (fst m2)) then None
  else
    let p1_prime = do_move m1 p1 in
    let p2_prime = do_move m2 p2 in
    
    if (vert (fst m1)) && (horz (fst m2)) then
      (* check that m2's y is in m1's vertical range *)
      let miny = min p1.y p1_prime.y in
      let maxy = max p1.y p1_prime.y in
      let vert_good = miny <= p2.y && p2.y <= maxy in

      (* check that m2 crossed over m1's x coord, i.e. the signed difference changed *)
      let old_sign = Int.sign (p2.x - p1.x) in
      let new_sign = Int.sign (p2_prime.x - p1.x) in
      let crossed_over = old_sign != new_sign in

      if vert_good && crossed_over then Some {x = p1.x; y = p2.y} else None
    else
      (* check that m1's y is in m2's vertical range *)
      let miny = min p2.y p2_prime.y in
      let maxy = max p2.y p2_prime.y in
      let vert_good = miny <= p1.y && p1.y <= maxy in

      (* check that m1 crossed over m2's x coord, i.e. the signed difference changed *)
      let old_sign = Int.sign (p1.x - p2.x) in
      let new_sign = Int.sign (p1_prime.x - p2.x) in
      let crossed_over = old_sign != new_sign in

      if vert_good && crossed_over then Some {x = p2.x; y = p1.y} else None
     
let run () =
  let wires = Util.read_all_lines "d3_input.txt"
              |> List.map ~f:parse_wire in
  List.iter wires ~f:(fun w -> Printf.printf "%s\n" (show_wire w));
