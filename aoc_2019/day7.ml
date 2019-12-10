open Core

let num_machines = 5

module State = struct
  type t = {
      mem : int Array.t;
      pc : int ref;
      input : int Lwt_mvar.t;
      output : int Lwt_mvar.t;
    }

  let init init_mem first_input = {
      mem = Array.of_list init_mem;
      pc = ref 0;
      input = Lwt_mvar.create first_input;
      output = Lwt_mvar.create_empty ();
    }

  (* Note: i is 1-indexed *)
  let read_param state i =
    let insn = state.mem.(!(state.pc)) in
    let div = Int.pow 10 (i + 1) in
    let param_mode = (insn / div) % 10 in
    match param_mode with
    | 0 -> state.mem.(state.mem.(!(state.pc) + i))
    | 1 -> state.mem.(!(state.pc) + i)
    | _ -> invalid_arg "Unknown parameter mode"

  (* Promise that resolves after one cpu iteration is complete *)
  let dispatch state : bool Lwt.t =
    match (state.mem.(!(state.pc)) % 100) with
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

let run_trial mem init_inputs =
  let machines = Sequence.range 0 num_machines
                 |> Sequence.map ~f:(fun i -> State.init mem (List.nth_exn init_inputs i))
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

let bigarr_to_list arr = List.init (Bigarray.Array1.dim arr) ~f:(fun i -> arr.{i})
                         |> List.rev

let run () =
  let input = Util.read_lines_to_string "d7_input.txt" in
  let data = String.split input ~on:','
             |> List.map ~f:int_of_string in
  (* Building this way because it looks like Permutation.to_list is busted *)
  let permutations = Combinat.Permutation.fold (Array.init num_machines ~f:ident)
    ~init:[] ~f:(fun acc new_perm -> (bigarr_to_list new_perm)::acc) in
  let p1 = List.map permutations ~f:(fun perm -> run_trial data perm)
           |> List.max_elt ~compare:Int.compare
           |> Option.value_exn in
  Printf.printf "Part 1: %d\n" p1;

