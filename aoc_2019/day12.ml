open Core
module Vec3 = Util.Vec3

(* Given two lists of vec3's, sum corresponding vectors in each index *)
let combine a b = List.zip_exn a b |> List.map ~f:(fun (v1, v2) -> Vec3.add v1 v2)

let signum3 a b =
  let to_int = function
    | Base__Sign0.Neg -> 1 (* a moves towards b *)
    | Base__Sign0.Pos -> -1 (* a moves away from b *)
    | Base__Sign0.Zero -> 0
  in
  let xs = Int.sign (a.Vec3.x - b.Vec3.x) in
  let ys = Int.sign (a.Vec3.y - b.Vec3.y) in
  let zs = Int.sign (a.Vec3.z - b.Vec3.z) in
  {Vec3.x = to_int xs; Vec3.y = to_int ys; Vec3.z = to_int zs}

(* How should the body `pos` move when affected by each `positions`? *)
let effect_on pos positions =
  List.map positions ~f:(fun other -> signum3 pos other)
  |> List.fold ~init:Vec3.origin ~f:Vec3.add

let next_vels positions = List.map positions ~f:(fun pos -> effect_on pos positions)

let run_round (positions, velocities) _ =
  let new_vels = combine velocities (next_vels positions) in
  let new_poss = combine positions new_vels in
  (new_poss, new_vels)

let energy positions velocities =
  List.zip_exn positions velocities
  |> List.map ~f:(fun (pos, vel) -> (Vec3.l1 pos) * (Vec3.l1 vel))
  |> List.fold ~init:0 ~f:(+)

let run () =
  let init_positions = Util.read_all_lines "d12_input.txt" |> List.map ~f:Vec3.parse in
  let init_vels = List.init (List.length init_positions) ~f:(fun _ -> Vec3.origin) in
  let (final_positions, final_vels) = Sequence.range 0 1000
                                      |> Sequence.fold ~init:(init_positions, init_vels) ~f:run_round in
  let p1 = energy final_positions final_vels in
  Printf.printf "Part 1: %d\n" p1
