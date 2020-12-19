let ipow base exp = int_of_float (Float.pow (float_of_int base) (float_of_int exp))

let range start stop =
  let f v =
    if v > stop then
      None
    else
      Some (v, v + 1)
  in
  Seq.unfold f start

let count_seq s = Seq.fold_left (fun acc _ -> acc + 1) 0 s

module Point = struct
  open Core
  type t = {
      x : int;
      y : int;
    }
             [@@deriving compare, sexp_of, hash, show]
  let origin = {x = 0; y = 0}

  let l1 p = abs p.x + abs p.y

  let equal a b = a.x = b.x && a.y = b.y

  let l2 {x = x1; y = y1} {x = x2; y = y2} =
    let sq = (ipow (x2 - x1) 2) + (ipow (y2 - y1) 2) in
    Float.sqrt (float_of_int sq)

  let l2_int a b = int_of_float (l2 a b)
end

module Vec3 = struct
  open Core
  type t =
    {
      x : int;
      y : int;
      z : int;
    }
  [@@deriving compare, sexp_of, hash, show]

  let origin = {x = 0; y = 0; z = 0}

  let l1 p = abs p.x + abs p.y + abs p.z

  let add a b = {x = a.x + b.x; y = a.y + b.y; z = a.z + b.z}

  let neg p = {x = -p.x; y = -p.y; z = -p.z}

  let equal a b = a.x = b.x && a.y = b.y && a.z = b.z

  let parse s =
    let rex = Re.Perl.compile_pat "<x=(.+), y=(.+), z=(.+)>" in
    let grp = Re.exec rex s in
    let matches = [1; 2; 3]
                  |> List.map ~f:(Re.Group.get grp)
                  |> List.map ~f:int_of_string in
    {x = List.nth_exn matches 0; y = List.nth_exn matches 1; z = List.nth_exn matches 2}
      
end

module Dir = struct
  type t =
    | Up
    | Down
    | Left
    | Right

  let vert = function
    | Up | Down -> true
    | _ -> false

  let horz d = not (vert d)

  let perpendicular a b =
    match [a; b] with
    | [Up; Down] -> false
    | [Down; Up] -> false
    | [Left; Right] -> false
    | [Right; Left] -> false
    | _ -> true

  let rot_right = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

  let rot_left = function
    | Up -> Left
    | Right -> Up
    | Down -> Right
    | Left -> Down

  let move ?(amt=1) p t =
    match t with
    | Up -> {p with Point.y = p.Point.y + amt}
    | Down -> {p with Point.y = p.Point.y - amt}
    | Left -> {p with Point.x = p.Point.x - amt}
    | Right -> {p with Point.x = p.Point.x + amt}
end

let read_all_lines name =
  let f = open_in name in
  let rec read_all acc =
    try read_all ((input_line f)::acc)
    with End_of_file -> List.rev acc
  in
  Fun.protect
    ~finally:(fun () -> close_in f)
    (fun () -> read_all [])

let read_lines_to_string name =
  let lst = read_all_lines name in
  String.concat "\n" lst
    
