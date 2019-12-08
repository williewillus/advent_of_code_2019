open Core

let width = 25
let height = 6
let layer_size = width * height

let count_value value layer = List.count ~f:((=) value) layer

let get_pixel layers idx =
  let rec _get_pixel = function
    | [] -> failwith "Every layer transparent!"
    | curlayer::rest ->
       match List.nth_exn curlayer idx with
       | 0 -> 0
       | 1 -> 1
       | 2 -> _get_pixel rest
       | _ as i -> failwith ("Invalid pixel value " ^ (string_of_int i))
  in
  _get_pixel layers

let collapse layers =
  Sequence.range 0 layer_size
  |> Sequence.map ~f:(get_pixel layers)
  |> Sequence.to_list

let repr v =
  match v with
  | 0 -> "\u{2588}" (* FULL BLOCK *)
  | 1 -> "\u{2591}" (* LIGHT SHADE *)
  | _ as i -> failwith ("Don't know how to show pixel value " ^ (string_of_int i))

let show layer =
  List.chunks_of layer ~length:width
  |> List.iter ~f:(fun row ->
         List.map row ~f:repr
         |> String.concat
         |> Printf.printf "%s\n")

let run () =
  let layers = String.concat (Util.read_all_lines "d8_input.txt")
               |> String.to_list
               |> List.map ~f:Char.get_digit_exn 
               |> List.chunks_of ~length:layer_size in
  let min_zero_layer = List.min_elt layers
                         ~compare:(fun l1 l2 -> Int.compare (count_value 0 l1) (count_value 0 l2)) 
                       |> Option.value_exn in
  let p1 = (count_value 1 min_zero_layer) * (count_value 2 min_zero_layer) in
  let () = Printf.printf "Part 1: %d\n" p1 in
  let () = Printf.printf "Part 2:\n" in
  let collapsed = collapse layers in
  show collapsed 
