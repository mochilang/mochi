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

let customers = [[("id",Obj.repr (1));("name",Obj.repr ("Alice"))];[("id",Obj.repr (2));("name",Obj.repr ("Bob"))]]
let orders = [[("id",Obj.repr (100));("customerId",Obj.repr (1))];[("id",Obj.repr (101));("customerId",Obj.repr (1))];[("id",Obj.repr (102));("customerId",Obj.repr (2))]]
let stats = (let __groups0 = ref [] in
  List.iter (fun o ->
      List.iter (fun c ->
              if (Obj.obj (List.assoc "customerId" o) = Obj.obj (List.assoc "id" c)) then (
        let key = Obj.obj (List.assoc "name" c) in
        let cur = try List.assoc key !__groups0 with Not_found -> [] in
        __groups0 := (key, o :: cur) :: List.remove_assoc key !__groups0);
      ) customers;
  ) orders;
  let __res0 = ref [] in
  List.iter (fun (gKey,gItems) ->
    let g = { key = gKey; items = List.rev gItems } in
    __res0 := [("name",Obj.repr (g.key));("count",Obj.repr (List.length g.items))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show ("--- Orders per customer ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | s::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "name" s)) ^ " " ^ __show ("orders:") ^ " " ^ __show (Obj.obj (List.assoc "count" s)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 stats with Break -> ()
