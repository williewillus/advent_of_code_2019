module Hashtbl = Base.Hashtbl
module Linked_queue = Base.Linked_queue
module Point = Util.Point

module Context = struct
  type obj =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball

  let obj_of_int = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | _ -> failwith "Unknown object type"
  
  type t =
    {
      state : Intcode.State.t;
      board : (Point.t, obj) Hashtbl.t;
      output_buffer : int Linked_queue.t;
    }

  let init data =
    let input_handler = fun _ -> failwith "Input unimplemented" in
    let out_buf = Linked_queue.create () in
    let output_handler = fun v -> Linked_queue.enqueue out_buf v in
    {
      state = Intcode.State.init ~input_handler ~output_handler data;
      board = Hashtbl.create (module Point);
      output_buffer = out_buf;
    }

  let run_to_output ctx =
    let res = Intcode.State.dispatch_until ctx.state
                (fun () -> Linked_queue.length ctx.output_buffer = 3) in
    let () = match res with
      | Intcode.State.Terminated -> ()
      | Intcode.State.ConditionMet ->
         let x = Linked_queue.dequeue_exn ctx.output_buffer in
         let y = Linked_queue.dequeue_exn ctx.output_buffer in
         let t = obj_of_int (Linked_queue.dequeue_exn ctx.output_buffer) in
         Hashtbl.set ctx.board ~key:{Point.x = x; y} ~data:t
    in
    res

  let run_to_complete ctx =
    let rec _loop () =
      let res = run_to_output ctx in
      match res with
      | Intcode.State.Terminated -> ctx
      | Intcode.State.ConditionMet -> _loop ()
    in
    _loop ()
end

let run () =
  let input = Util.read_lines_to_string "d13_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map int_of_string in
  let ctx = Context.run_to_complete (Context.init data) in
  let p1 = Hashtbl.count ctx.board ~f:(fun v -> v = Block) in
  Printf.printf "Part 1: %d\n" p1
