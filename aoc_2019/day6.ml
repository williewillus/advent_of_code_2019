open Core

let parse_pair line =
  let split = String.split line ~on:')' in
  (List.nth_exn split 0, List.nth_exn split 1)

let path_to parents src dest =
  (* precondition: acc already contains cur *)
  let rec _path acc cur =
    if cur = dest then acc else
      match Hashtbl.find parents cur with
      | Some parent -> _path (parent::acc) parent
      | None -> failwith "Should never get here"
  in
  List.rev (_path [src] src)

let dist_to parents src dest = List.length (path_to parents src dest) - 1

let height parents node = dist_to parents node "COM"

let common_ancestor path_a path_b =
  let b_set = Hash_set.of_list (module String) path_b in
  List.find path_a ~f:(Hash_set.mem b_set)
  |> Option.value_exn ~message:"No common ancestor!"

let run () =
  let parents = Util.read_all_lines "d6_input.txt"
              |> List.map ~f:parse_pair 
              |> List.fold ~init:(Hashtbl.create (module String))
                   ~f:(fun acc (orbitee, orbiter) ->
                     if Hashtbl.mem acc orbiter then
                       failwith (orbiter ^ " is orbiting two things?!")
                     else
                       let () = Hashtbl.set acc ~key:orbiter ~data:orbitee in
                       acc
                   ) in
  let p1 = Hashtbl.keys parents
           |> List.sum (module Int) ~f:(fun k -> height parents k) in
  let () = Printf.printf "Part 1: %d\n" p1 in
  let ancestor = common_ancestor (path_to parents "YOU" "COM") (path_to parents "SAN" "COM") in
  let you_parent = Hashtbl.find_exn parents "YOU" in
  let san_parent = Hashtbl.find_exn parents "SAN" in
  let p2 = (dist_to parents you_parent ancestor) + (dist_to parents san_parent ancestor) in
  Printf.printf "Part 2: %d\n" p2
  
