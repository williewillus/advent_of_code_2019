open Core

let coeffs_cache = (Hashtbl.create (module Int))
let coeffs idx =
  let lst = Hashtbl.find_or_add coeffs_cache idx
              ~default:(fun () ->
                let z = List.init (idx + 1) ~f:(fun _ -> 0) in
                let o = List.init (idx + 1) ~f:(fun _ -> 1) in
                let no = List.init (idx + 1) ~f:(fun _ -> -1) in
                List.concat [z; o; z; no]) in
  Sequence.drop_eagerly (Sequence.cycle_list_exn lst) 1

let show lst = List.map ~f:string_of_int lst |> String.concat

let iter lst =
  List.mapi lst ~f:(fun idx _ ->
      let coef = coeffs idx in
      let dot = Sequence.zip (Sequence.of_list lst) coef
                |> Sequence.map ~f:(fun (a, b) -> a * b)
                |> Sequence.fold ~init:0 ~f:( + ) in
      abs (dot mod 10))  

let run () =
  let digits = String.concat (Util.read_all_lines "d16_input.txt")
               |> String.to_list 
               |> List.map ~f:Char.get_digit_exn in
  let seq = Sequence.unfold ~init:digits
    ~f:(fun prev ->
      let next = iter prev in
      Some (next, next)) in
  let p1 = Sequence.nth_exn seq 99 in
  Printf.printf "Part 1: %s\n" (show (List.slice p1 0 8))
