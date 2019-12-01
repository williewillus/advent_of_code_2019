open Core

let read_all_lines name =
  let f = In_channel.create name in
  protect
    ~f:(fun () -> In_channel.input_lines f)
    ~finally:(fun () -> In_channel.close f)

let read_lines_to_string name =
  let lst = read_all_lines name in
  String.concat ~sep:"\n" lst
    
