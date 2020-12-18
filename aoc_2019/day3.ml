module Dir = Util.Dir
module Point = Util.Point         

type move = (Dir.t * int)
let do_move m p = Dir.move ~amt:(snd m) p (fst m)

let sign i = compare i 0

let parse_move s : move =
  let direction = String.get s 0 in
  let amount = int_of_string (String.sub s 1 (String.length s - 1)) in
  match direction with
  | 'L' -> (Left, amount)
  | 'R' -> (Right, amount)
  | 'U' -> (Up, amount)
  | 'D' -> (Down, amount)
  |  _  -> invalid_arg "Unknown direction"

let parse_wire line : move list =
  String.split_on_char ',' line
  |> List.map parse_move

module Intersection = struct
  type t =
    {
      pos : Point.t;
      dist1 : int;
      dist2 : int;
    }
end

(* when point p1 makes move m1, and point p2 makes move m2, does an intersection occur? 
   if so, returns the point of intersection, else none *)
let one_one_intersect dist1 p1 m1 dist2 p2 m2 =
  if not (Dir.perpendicular (fst m1) (fst m2)) then None (* XXX: Doesn't support parallel intersects *)
  else
    let p1_prime = do_move m1 p1 in
    let p2_prime = do_move m2 p2 in
    
    if (Dir.vert (fst m1)) && (Dir.horz (fst m2)) then
      (* check that m2's y is in m1's vertical range *)
      let miny = min p1.y p1_prime.y in
      let maxy = max p1.y p1_prime.y in
      let vert_good = miny <= p2.y && p2.y <= maxy in

      (* check that m2 crossed over m1's x coord, i.e. the signed difference changed *)
      let old_sign = sign (p2.x - p1.x) in
      let new_sign = sign (p2_prime.x - p1.x) in
      let crossed_over = old_sign <> new_sign in

      let pos = {Point.x = p1.x; y = p2.y} in
      let new_dist1 = dist1 + Point.l2_int p1 pos in
      let new_dist2 = dist2 + Point.l2_int p2 pos in

      if vert_good && crossed_over then
        Some {Intersection.pos = pos; dist1 = new_dist1; dist2 = new_dist2}
      else None
    else
      (* check that m1's y is in m2's vertical range *)
      let miny = min p2.y p2_prime.y in
      let maxy = max p2.y p2_prime.y in
      let vert_good = miny <= p1.y && p1.y <= maxy in

      (* check that m1 crossed over m2's x coord, i.e. the signed difference changed *)
      let old_sign = sign (p1.x - p2.x) in
      let new_sign = sign (p1_prime.x - p2.x) in
      let crossed_over = old_sign <> new_sign in

      let pos = {Point.x = p2.x; y = p1.y} in
      let new_dist1 = dist1 + Point.l2_int p1 pos in
      let new_dist2 = dist2 + Point.l2_int p2 pos in

      if vert_good && crossed_over then
        Some {Intersection.pos = pos; dist1 = new_dist1; dist2 = new_dist2}
      else None

let one_all_intersect dist1 p1 m1 m2s : Intersection.t list =
  let f (acc, dist2, p2) m2 =
    let nacc = match one_one_intersect dist1 p1 m1 dist2 p2 m2 with
      | Some intersection -> intersection::acc
      | None -> acc
    in
    (nacc, dist2 + (snd m2), do_move m2 p2)
  in
  let (intersections, _, _) = List.fold_left f ([], 0, Point.origin) m2s in
  intersections

let all_all_intersect m1s m2s : Intersection.t list =
  let f (acc, dist1, p1) m1 =
    let step = one_all_intersect dist1 p1 m1 m2s in
    (step @ acc, dist1 + (snd m1), do_move m1 p1)
  in
  let (intersections, _, _) = List.fold_left f ([], 0, Point.origin) m1s in
  intersections
     
let run () =
  let wires = Util.read_all_lines "d3_input.txt"
              |> List.map parse_wire in
  let all = all_all_intersect (List.nth wires 0) (List.nth wires 1) in (* XXX: only supports 2 *)
  let p1 = List.map (fun i -> i.Intersection.pos) all
           |> List.sort (fun a b -> (Point.l1 a) - (Point.l1 b))
           |> List.hd in
  let () = Printf.printf "Part 1: Closest %s with distance %d\n" (Point.show p1) (Point.l1 p1) in

  let p2 = List.sort
             (fun i1 i2 ->
               (i1.Intersection.dist1 + i1.Intersection.dist2)
               - (i2.Intersection.dist1 + i2.Intersection.dist2))
             all
           |> List.hd in
  Printf.printf "Part 2: First intersection in %d steps\n"
    (p2.Intersection.dist1 + p2.Intersection.dist2)
