open Core

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right

  let vert d = d = Up || d = Down
  let horz d = d = Left || d = Right

  let perpendicular a b =
    match [a; b] with
    | [Up; Down] -> false
    | [Down; Up] -> false
    | [Left; Right] -> false
    | [Right; Left] -> false
    | _ -> true
end
  
module Point = struct
  type t = {
      x : int;
      y : int;
    }
             [@@deriving compare, sexp_of, hash, show]
  let origin = {x = 0; y = 0}

  let l1 p = abs p.x + abs p.y
end

module Result = struct
  type t = (Point.t * int)
  [@@deriving compare, sexp_of, hash] 
end

type move = (Dir.t * int)
let do_move m p =
  let open Point in
  match (fst m) with
  | Dir.Up -> {p with y = p.y + (snd m)}
  | Dir.Down -> {p with y = p.y - (snd m)}
  | Dir.Left -> {p with x = p.x - (snd m)}
  | Dir.Right -> {p with x = p.x + (snd m)}


let parse_move s : move =
  let direction = String.get s 0 in
  let amount = int_of_string (String.sub s ~pos:1 ~len:(String.length s - 1)) in
  match direction with
    'L' -> (Left, amount)
  | 'R' -> (Right, amount)
  | 'U' -> (Up, amount)
  | 'D' -> (Down, amount)
  |  _  -> invalid_arg "Unknown direction"

let parse_wire line : move list =
  String.split line ~on:','
  |> List.map ~f:parse_move

(* Every wire is a list of moves (mag + dir)
   for each move (tracking current position), iterate through moves of every other wire
   intersection between m1, m2 if 
   1. directions are perpendicular AND
   2. m1 is vert and m2 is horz -> m2.y contained in m1.y-m1.y' and m2.x passes over m1.x
   3. vice versa 2
*)

(* when point p1 makes move m1, and point p2 makes move m2, does an intersection occur? 
   if so, returns the point of intersection, else none *)
let one_one_intersect p1 m1 p2 m2 : Point.t option =
  if not (Dir.perpendicular (fst m1) (fst m2)) then None
  else
    let p1_prime = do_move m1 p1 in
    let p2_prime = do_move m2 p2 in
    
    if (Dir.vert (fst m1)) && (Dir.horz (fst m2)) then
      (* check that m2's y is in m1's vertical range *)
      let miny = min p1.y p1_prime.y in
      let maxy = max p1.y p1_prime.y in
      let vert_good = miny <= p2.y && p2.y <= maxy in

      (* check that m2 crossed over m1's x coord, i.e. the signed difference changed *)
      let old_sign = Int.sign (p2.x - p1.x) in
      let new_sign = Int.sign (p2_prime.x - p1.x) in
      let crossed_over = old_sign <> new_sign in

      if vert_good && crossed_over then Some {x = p1.x; y = p2.y} else None
    else
      (* check that m1's y is in m2's vertical range *)
      let miny = min p2.y p2_prime.y in
      let maxy = max p2.y p2_prime.y in
      let vert_good = miny <= p1.y && p1.y <= maxy in

      (* check that m1 crossed over m2's x coord, i.e. the signed difference changed *)
      let old_sign = Int.sign (p1.x - p2.x) in
      let new_sign = Int.sign (p1_prime.x - p2.x) in
      let crossed_over = old_sign <> new_sign in

      if vert_good && crossed_over then Some {x = p2.x; y = p1.y} else None

let one_all_intersect p1 m1 (m2s : move list) : (int * Point.t) list =
  let f (acc, dist2, p2) m2 =
    let new_dist2 = dist2 + (snd m2) in
    let new_p2 = do_move m2 p2 in
    match one_one_intersect p1 m1 p2 m2 with
      Some intersection -> ((new_dist2, intersection) :: acc, new_dist2, new_p2)
    | None -> (acc, new_dist2, new_p2)
  in
  let (intersections, _, _) = List.fold m2s ~init:([], 0, Point.origin) ~f in
  intersections

let all_all_intersect (m1s : move list) (m2s : move list) : (int * int * Point.t) list =
  let f (acc, dist1, p1) m1 =
    let new_dist1 = dist1 + (snd m1) in
    let step = one_all_intersect p1 m1 m2s
               |> List.map ~f:(fun (dist2, p) -> (new_dist1, dist2, p)) in
    (step @ acc, new_dist1, do_move m1 p1)
  in
  let (intersections, _, _) = List.fold m1s ~init:([], 0, Point.origin) ~f in
  intersections
     
let run () =
  let wires = Util.read_all_lines "d3_input.txt"
              |> List.map ~f:parse_wire in
  let all = all_all_intersect (List.nth_exn wires 0) (List.nth_exn wires 1) in
  let p1 = List.map all ~f:(fun (_, _, p) -> p)
           |> List.sort ~compare:(fun a b -> (Point.l1 a) - (Point.l1 b))
           |> List.remove_consecutive_duplicates ~equal:(=)
           |> List.hd_exn in
  let () = Printf.printf "Part 1: Closest %s with distance %d\n" (Point.show p1) (Point.l1 p1) in
  let (dist1, dist2, _) = List.sort all
                            ~compare:(fun (d1a, d2a, _) (d1b, d2b, _) -> (d1a + d2a) - (d1b + d2b))
                          |> List.hd_exn in
  let () = Printf.printf "Part 2: First intersection in %d steps\n" (dist1 + dist2) in
  ()
