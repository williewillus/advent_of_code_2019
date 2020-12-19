let parse_pair line =
  let split = String.split_on_char ')' line in
  (List.nth split 0, List.nth split 1)

let path_to parents src dest =
  (* precondition: acc already contains cur *)
  let rec _path acc cur =
    if String.equal cur dest then acc else
      let parent = Hashtbl.find parents cur in
      _path (parent::acc) parent
  in
  List.rev (_path [src] src)

let dist_to parents src dest = List.length (path_to parents src dest) - 1

let height parents node = dist_to parents node "COM"

let common_ancestor path_a path_b =
  let module Hash_set = Base.Hash_set in
  let b_set = Hash_set.of_list (module Base.String) path_b in
  List.find (Hash_set.mem b_set) path_a

let run () =
  let parents = Util.read_all_lines "d6_input.txt"
              |> List.map parse_pair 
              |> List.fold_left 
                   (fun acc (orbitee, orbiter) ->
                     if Hashtbl.mem acc orbiter then
                       failwith (orbiter ^ " is orbiting two things?!")
                     else
                       let () = Hashtbl.add acc orbiter orbitee in
                       acc
                   )
                   (Hashtbl.create 0)
  in
  let p1 = Hashtbl.to_seq_keys parents
           |> Seq.map (fun k -> height parents k)
           |> Seq.fold_left (+) 0 in
  let () = Printf.printf "Part 1: %d\n" p1 in
  let ancestor = common_ancestor (path_to parents "YOU" "COM") (path_to parents "SAN" "COM") in
  let you_parent = Hashtbl.find parents "YOU" in
  let san_parent = Hashtbl.find parents "SAN" in
  let p2 = (dist_to parents you_parent ancestor) + (dist_to parents san_parent ancestor) in
  Printf.printf "Part 2: %d\n" p2
  
