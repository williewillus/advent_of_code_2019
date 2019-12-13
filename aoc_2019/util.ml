open Core

module Point = struct
  type t = {
      x : int;
      y : int;
    }
             [@@deriving compare, sexp_of, hash, show]
  let origin = {x = 0; y = 0}

  let l1 p = abs p.x + abs p.y

  let equal a b = a.x = b.x && a.y = b.y
end

module Vec3 = struct
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

  let move p t =
    match t with
    | Up -> {p with Point.y = p.Point.y + 1}
    | Down -> {p with Point.y = p.Point.y - 1}
    | Left -> {p with Point.x = p.Point.x - 1}
    | Right -> {p with Point.x = p.Point.x + 1}
end

let read_all_lines name =
  let f = In_channel.create name in
  protect
    ~f:(fun () -> In_channel.input_lines f)
    ~finally:(fun () -> In_channel.close f)

let read_lines_to_string name =
  let lst = read_all_lines name in
  String.concat ~sep:"\n" lst
    
