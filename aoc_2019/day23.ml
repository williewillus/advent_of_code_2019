module Point = Util.Point

type sendstate =
  | NeedAddr
  | NeedX of int
  | NeedY of (int * int)

type recvstate =
  | NeedAddress
  | Fresh
  | NeedY of Point.t

type computer = {
    id : int;
    rx : Point.t Queue.t;
    rxstate : recvstate ref;
    txstate : sendstate ref
  }

let input_handler comp () =
  match !(comp.rxstate) with
  | NeedAddress ->
     comp.rxstate := Fresh;
     Printf.printf "%d: get id" comp.id;
     comp.id
  | Fresh ->
     if Queue.is_empty comp.rx then -1
     else
       let p = Queue.pop comp.rx in
       comp.rxstate := NeedY p;
       Printf.printf "%d: read x %d" comp.id p.x;
       p.x
  | NeedY p ->
     comp.rxstate := Fresh;
     Printf.printf "%d: read y %d" comp.id p.y;
     p.y

let output_handler output_queue_getter comp v =
  match !(comp.txstate) with
  | NeedAddr ->
     comp.txstate := NeedX v
  | NeedX addr ->
     comp.txstate := NeedY (addr, v)
  | NeedY (addr, x) ->
     let rx = output_queue_getter addr in
     let () = Queue.push { Point.x = x; y = v } rx in
     comp.txstate := NeedAddr

let run () =
  let input = Util.read_lines_to_string "d23_input.txt" in
  let image = String.split_on_char ',' input
             |> List.map int_of_string in
  let comps = Array.init 50 (fun i ->
                  { id = i; rx = Queue.create (); txstate = ref NeedAddr; rxstate = ref Fresh }) in
  let out = Queue.create () in
  let output_queue_getter addr = if addr = 255 then out else comps.(addr).rx in
  let states = Array.map
                 (fun c ->
                   let input_handler = input_handler c in
                   let output_handler = output_handler output_queue_getter c in
                   Intcode.State.init ~input_handler ~output_handler image)
                 comps in
  while Queue.is_empty out do
    print_endline "outer loop";
    Array.iteri
      (fun i s ->
        let x = Intcode.State.dispatch s in
        if x then
          Printf.printf "Machine %d exited?\n" i;)
      states
  done;
  Printf.printf "Part 1: %d" (Queue.peek out).y; 
  ()
