module Array = Core_kernel.Array
module List = Base.List
module Sequence = Base.Sequence

let num_machines = 5

module State = struct
  type t = {
      id : int;
      mem : int Array.t;
      pc : int ref;
      input : int Lwt_mvar.t;
      output : int Lwt_mvar.t;
    }

  let init id init_mem first_input = {
      id;
      mem = Array.of_list init_mem;
      pc = ref 0;
      input = Lwt_mvar.create first_input;
      output = Lwt_mvar.create_empty ();
    }

  (* Note: i is 1-indexed *)
  let read_param state i =
    let insn = state.mem.(!(state.pc)) in
    let div = Base.Int.pow 10 (i + 1) in
    let param_mode = (insn / div) mod 10 in
    match param_mode with
    | 0 -> state.mem.(state.mem.(!(state.pc) + i))
    | 1 -> state.mem.(!(state.pc) + i)
    | _ -> invalid_arg "Unknown parameter mode"

  (* Promise that resolves after one cpu iteration is complete *)
  let dispatch state : bool Lwt.t =
    match (state.mem.(!(state.pc)) mod 100) with
    | 1 -> begin
        let l = read_param state 1 in 
        let r = read_param state 2 in 
        let dest = state.mem.(!(state.pc) + 3) in
        state.mem.(dest) <- l + r;
        state.pc := !(state.pc) + 4;
        Lwt.return false
      end
    | 2 -> begin
        let l = read_param state 1 in 
        let r = read_param state 2 in 
        let dest = state.mem.(!(state.pc) + 3) in
        state.mem.(dest) <- l * r;
        state.pc := !(state.pc) + 4;
        Lwt.return false
      end
    | 3 ->
       let%lwt v = Lwt_mvar.take state.input in
       let dest = state.mem.(!(state.pc) + 1) in
       state.mem.(dest) <- v;
       state.pc := !(state.pc) + 2;
       Lwt.return false
    | 4 ->
       let v = read_param state 1 in
       let%lwt () = Lwt_mvar.put state.output v in
       state.pc := !(state.pc) + 2;
       Lwt.return false
    | 5 ->
       let tst = read_param state 1 in
       let dest = read_param state 2 in
       if tst <> 0 then
         state.pc := dest
       else
         state.pc := !(state.pc) + 3;
       Lwt.return false
    | 6 ->
       let tst = read_param state 1 in
       let dest = read_param state 2 in
       if tst = 0 then
         state.pc := dest
       else
         state.pc := !(state.pc) + 3;
       Lwt.return false
    | 7 ->
       let a = read_param state 1 in
       let b = read_param state 2 in
       let dest = state.mem.(!(state.pc) + 3) in
       state.mem.(dest) <- if a < b then 1 else 0;
       state.pc := !(state.pc) + 4;
       Lwt.return false
    | 8 ->
       let a = read_param state 1 in
       let b = read_param state 2 in
       let dest = state.mem.(!(state.pc) + 3) in
       state.mem.(dest) <- if a = b then 1 else 0;
       state.pc := !(state.pc) + 4;
       Lwt.return false
    | 99 -> Lwt.return true
    | _ as i -> invalid_arg ("Unknown opcode" ^ (string_of_int i))

  (* Returns a promise that resolves only when this abstract machine terminates,
     i.e. dispatches instruction 99
   *)
  let run_to_end state =
    let rec iter () =
      let%lwt stop = dispatch state in
      if stop then Lwt.return_unit else iter ()
    in
    iter ()
    
end

(* Returns a coroutine that infinitely transfers items from in_mbox to out_mbox *)
let pipe in_mbox out_mbox = 
  let rec do_transfer () =
    let%lwt v = Lwt_mvar.take in_mbox in
    let%lwt () = Lwt_mvar.put out_mbox v in
    do_transfer ()
  in
  do_transfer ()

(* Returns a coroutine that infinitely supplies value v to out_mbox *)
let inf_supplier v out_mbox =
  let rec do_transfer () =
    let%lwt () = Lwt_mvar.put out_mbox v in
    do_transfer ()
  in
  do_transfer ()

let run_trial_p1 mem init_inputs =
  let machines = Sequence.range 0 num_machines
                 |> Sequence.map ~f:(fun i -> State.init i mem (List.nth_exn init_inputs i))
                 |> Sequence.to_array in

  (* Launch a coroutine for each machine *)
  let () = Array.iter machines ~f:(fun m -> Lwt.async (fun () -> State.run_to_end m)) in

  (* Launch a coroutine supplying 0's to first machine *)
  let () = Lwt.async (fun () -> inf_supplier 0 machines.(0).State.input) in

  (* Launch a coroutine piping between all the other machines *)
  let () = Sequence.range 0 (num_machines - 1)
           |> Sequence.map ~f:(fun i -> pipe machines.(i).output machines.(i+1).input)
           |> Sequence.iter ~f:(fun prom -> Lwt.async (fun () -> prom)) in

  (* Drive scheduler until we get output of last abstract machine *)
  Lwt_main.run (Lwt_mvar.take machines.(num_machines - 1).State.output)

let run_trial_p2 mem init_inputs =
  let machines = Sequence.range 0 num_machines
                 |> Sequence.map ~f:(fun i -> State.init i mem (List.nth_exn init_inputs i))
                 |> Sequence.to_array in

  (* Launch a coroutine for each machine except the last *)
  let () = Array.slice machines 0 (num_machines - 1)
           |> Array.iter ~f:(fun m -> Lwt.async (fun () -> State.run_to_end m)) in

  (* Launch a coroutine piping between all the other machines *)
  let () = Sequence.range 0 (num_machines - 1)
           |> Sequence.map ~f:(fun i -> pipe machines.(i).output machines.(i+1).input)
           |> Sequence.iter ~f:(fun prom -> Lwt.async (fun () -> prom)) in

  (* Promise resolving when last machine terminates/the run is complete *)
  let last_machine_prom = State.run_to_end machines.(num_machines - 1) in
  let () = Lwt.async (fun () -> last_machine_prom) in

  let top_level =
    (* Initial zero to first machine *)
    let%lwt () = Lwt_mvar.put machines.(0).State.input 0 in

    (* Repeatedly pipe outputs from last machine to first until the machine terminates
       But hold onto the values for the results *)
    let rec _loop (acc : int list) =
      match Lwt.state last_machine_prom with
      | Lwt.Sleep ->
         let%lwt v = Lwt_mvar.take machines.(num_machines - 1).State.output in
         let%lwt () = Lwt_mvar.put machines.(0).State.input v in
         (_loop (v::acc))
      | _ ->
         (* HACK! If last machine puts uncontended then immediately terminates this coroutine can
            "miss" the final value. Ideally we would loop until the "channel is closed" instead
            of polling the last machine coroutine's state, but mvar does not present such a
            "channel closed" abstraction.
          *)
         match Lwt_mvar.take_available machines.(num_machines - 1).State.output with
         | Some v -> Lwt.return (v::acc)
         | None -> Lwt.return acc
    in
    
    let%lwt outputs = _loop [] in
    Lwt.return (List.max_elt ~compare:Int.compare outputs |> Option.get)
  in
    
  (* Drive scheduler until top_level resolves *)
  Lwt_main.run top_level


let bigarr_to_list arr = List.init (Bigarray.Array1.dim arr) ~f:(fun i -> arr.{i})
                         |> List.rev

let run () =
  let input = Util.read_lines_to_string "d7_input.txt" in
  let data = String.split_on_char ',' input
             |> List.map ~f:int_of_string in

  (* Building this way because it looks like Permutation.to_list is busted *)
  let p1_permutations = Combinat.Permutation.fold (Array.init num_machines ~f:(fun i -> i))
    ~init:[] ~f:(fun acc new_perm -> (bigarr_to_list new_perm)::acc) in
  let p1 = List.map p1_permutations ~f:(fun perm -> run_trial_p1 data perm)
           |> List.max_elt ~compare:Int.compare
           |> Option.get in
  Printf.printf "Part 1: %d\n" p1;

  let p2_permutations = Combinat.Permutation.fold (Array.init num_machines ~f:(fun i -> i + 5))
    ~init:[] ~f:(fun acc new_perm -> (bigarr_to_list new_perm)::acc) in
  let p2 = List.map p2_permutations ~f:(fun perm -> run_trial_p2 data perm)
           |> List.max_elt ~compare:Int.compare
           |> Option.get in
  Printf.printf "Part 2: %d\n" p2;
