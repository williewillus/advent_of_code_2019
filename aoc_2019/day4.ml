open Core

let digits i =
  let rec _digits acc i =
    if i = 0 then acc else _digits ((i % 10)::acc) (i / 10)
  in
  _digits [] i

let valid_p1 i =
  let d = digits i in
  let order_good = List.is_sorted d ~compare:Int.compare in
  let double_good = Option.is_some (List.find_consecutive_duplicate d ~equal:(=)) in
  order_good && double_good

let valid_p2 i =
  let d = digits i in
  let order_good = List.is_sorted d ~compare:Int.compare in
  let runs = List.group d ~break:(<>) in (* extracts all consecutive runs of digits *)
  let groups_good = List.exists runs ~f:(fun group -> (List.length group) = 2) in
  order_good && groups_good

let run () =
  let seq = Sequence.range ~start:`inclusive ~stop:`inclusive 178416 676461 in
  let p1 = Sequence.count seq ~f:valid_p1 in
  let p2 = Sequence.count seq ~f:valid_p2 in
  Printf.printf "Part 1: %d\n" p1;
  Printf.printf "Part 2: %d\n" p2;
