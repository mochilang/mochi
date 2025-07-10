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

  type record1 = { mutable id : int; mutable name : string }
  type record2 = { mutable id : int; mutable customerId : int }
  type record3 = { mutable name : Obj.t; mutable count : int }

let customers : record1 list = [{ id = 1; name = "Alice" };{ id = 2; name = "Bob" };{ id = 3; name = "Charlie" }]
let orders : record2 list = [{ id = 100; customerId = 1 };{ id = 101; customerId = 1 };{ id = 102; customerId = 2 }]
let stats : (string * Obj.t) list list = (let __groups0 = ref [] in
  List.iter (fun c ->
      List.iter (fun o ->
              if (Obj.obj (List.assoc "customerId" o) = Obj.obj (List.assoc "id" c)) then (
        let key = Obj.obj (List.assoc "name" c) in
        let cur = try List.assoc key !__groups0 with Not_found -> [] in
        __groups0 := (key, c :: cur) :: List.remove_assoc key !__groups0);
      ) orders;
  ) customers;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := { name = g.key; count = List.length (let __res1 = ref [] in
  List.iter (fun r ->
      if Obj.obj (List.assoc "o" r) then
    __res1 := r :: !__res1;
  ) g.items;
List.rev !__res1)
 } :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline "--- Group Left Join ---";
  let rec __loop2 lst =
    match lst with
      | [] -> ()
      | s::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "name" s)) ^ " " ^ __show ("orders:") ^ " " ^ __show (Obj.obj (List.assoc "count" s)));
        with Continue -> ()
        ; __loop2 rest
    in
    try __loop2 stats with Break -> ()
