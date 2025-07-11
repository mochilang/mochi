  let rec __show v =
    let open Obj in
    let rec list_aux o =
      if is_int o && (magic (obj o) : int) = 0 then "" else
       let hd = field o 0 in
       let tl = field o 1 in
       let rest = list_aux tl in
       if rest = "" then __show (obj hd) else __show (obj hd) ^ "; " ^ rest
    in
    let r = repr v in
    if is_int r then string_of_int (magic v) else
    match tag r with
      | 0 -> if size r = 0 then "[]" else "[" ^ list_aux r ^ "]"
      | 252 -> (magic v : string)
      | 253 -> string_of_float (magic v)
      | _ -> "<value>"

  exception Break
  exception Continue

  type ('k,'v) group = { key : 'k; items : 'v list }

  type record1 = { mutable name : string; mutable age : int; mutable city : string }
  type record2 = { mutable city : Obj.t; mutable count : int; mutable avg_age : float }

let people : record1 list = [{ name = "Alice"; age = 30; city = "Paris" };{ name = "Bob"; age = 15; city = "Hanoi" };{ name = "Charlie"; age = 65; city = "Paris" };{ name = "Diana"; age = 45; city = "Hanoi" };{ name = "Eve"; age = 70; city = "Paris" };{ name = "Frank"; age = 22; city = "Hanoi" }]
let stats : record2 list = (let __groups0 = ref [] in
  List.iter (fun person ->
      let key = Obj.obj (List.assoc "city" person) in
      let cur = try List.assoc key !__groups0 with Not_found -> [] in
      __groups0 := (key, person :: cur) :: List.remove_assoc key !__groups0;
  ) people;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { city = g.key; count = List.length g.items; avg_age = (List.fold_left (+) 0 (let __res1 = ref [] in
  List.iter (fun p ->
      __res1 := Obj.obj (List.assoc "age" p) :: !__res1;
  ) g.items;
List.rev !__res1)
 / List.length (let __res1 = ref [] in
  List.iter (fun p ->
      __res1 := Obj.obj (List.assoc "age" p) :: !__res1;
  ) g.items;
List.rev !__res1)
) } :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline "--- People grouped by city ---";
  let rec __loop2 lst =
    match lst with
      | [] -> ()
      | s::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "city" s)) ^ " " ^ __show (": count =") ^ " " ^ __show (Obj.obj (List.assoc "count" s)) ^ " " ^ __show (", avg_age =") ^ " " ^ __show (Obj.obj (List.assoc "avg_age" s)));
        with Continue -> ()
        ; __loop2 rest
    in
    try __loop2 stats with Break -> ()
