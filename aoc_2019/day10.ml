open Core
module Point = Util.Point

let all_colinear asteroids a b : Point.t list =
  let slope = (float_of_int (b.Point.y - a.Point.y)) /. (float_of_int (b.Point.x - a.Point.x)) in
  let yint = (float_of_int a.Point.y) -. ((float_of_int a.Point.x) *. slope) in

  let ret = Hash_set.fold asteroids ~init:[] ~f:(fun acc aster ->
      if Point.equal a aster || Point.equal b aster then acc (* Don't eliminate the pair under test*)
      else if Float.is_inf slope then (* Special case for vertical slope *)
        if aster.Point.x = b.Point.x then aster::acc else acc
      else if Point.compare a b <> Point.compare a aster then acc (* Don't include asteroids behind us *)
      else
        (* Check if it's on the line *)
        let open Poly in
        let x = float_of_int aster.Point.x in
        let y = float_of_int aster.Point.y in
        let test_y = x *. slope +. yint in
        if Float.abs (test_y -. y) < 0.000001 then aster::acc else acc)
  in
  let () = Printf.printf "\ncolinear with %s/%s == %s\n" (Point.show a) (Point.show b)
             (String.concat ~sep:"," (List.map ~f:Point.show ret)) in
  ret

let visible_asteroids asteroids src =
    let eliminated = Hash_set.create (module Point) in
    let () = Hash_set.to_list asteroids
             |> List.sort ~compare:(fun a b -> Float.compare (Point.l2 src a) (Point.l2 src b))
             |> List.iter
                  ~f:(fun dest ->
                    if not (Hash_set.mem eliminated dest) then
                                    all_colinear asteroids src dest
                                    |> List.iter ~f:(Hash_set.add eliminated))
    in
    let ret = Hash_set.length asteroids - Hash_set.length eliminated in
    let () = Printf.printf "%s src %d aster %d elm = %d\n" (Point.show src) (Hash_set.length asteroids) (Hash_set.length eliminated) ret in
    ret

let run () =
  let input = Util.read_all_lines "d10_input.txt" |> List.map ~f:String.to_list in
  (*let height = List.length input in
  let width = List.length (List.nth_exn input 0) in *)
  let grid = Hash_set.create (module Point) in
  let () = List.iteri input ~f:(fun y line ->
                 List.iteri line ~f:(fun x ch ->
                     match ch with
                     | '#' -> Hash_set.add grid {Point.x = x; Point.y = y}
                     | '.' -> ()
                     | _ -> failwith "Unknown marker")) in

  (* let all_points = Sequence.range 0 height
   *                  |> Sequence.map ~f:(fun y -> Sequence.range 0 width
   *                                               |> Sequence.map ~f:(fun x -> {Point.x = x; Point.y = y}))
   *                  |> Sequence.concat
   *                  |> Sequence.to_list in
   * let p1 = List.map all_points ~f:(visible_asteroids grid)
   *          |> List.max_elt ~compare:Int.compare
   *          |> Option.value_exn in
   * Printf.printf "Part 1: %d\n" p1 *)
  Printf.printf "AAAAAAAAA: %d\n" (visible_asteroids grid {Point.x = 5; y = 8})
