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

let customers = [[("id",Obj.repr (1));("name",Obj.repr ("Alice"))];[("id",Obj.repr (2));("name",Obj.repr ("Bob"))];[("id",Obj.repr (3));("name",Obj.repr ("Charlie"))]]
let orders = [[("id",Obj.repr (100));("customerId",Obj.repr (1))];[("id",Obj.repr (101));("customerId",Obj.repr (1))];[("id",Obj.repr (102));("customerId",Obj.repr (2))]]
let stats = (let __groups0 = ref [] in
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
    __res0 := [("name",Obj.repr (g.key));("count",Obj.repr (List.length (let __res1 = ref [] in
  List.iter (fun r ->
      if Obj.obj (List.assoc "o" r) then
    __res1 := r :: !__res1;
  ) g.items;
List.rev !__res1)
))] :: !__res0
  ) !__groups0;
  List.rev !__res0)


let () =
  print_endline (__show ("--- Group Left Join ---"));
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
