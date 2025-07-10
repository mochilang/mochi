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


let customers = [[("id",Obj.repr (1));("name",Obj.repr ("Alice"))];[("id",Obj.repr (2));("name",Obj.repr ("Bob"))]]
let orders = [[("id",Obj.repr (100));("customerId",Obj.repr (1))];[("id",Obj.repr (101));("customerId",Obj.repr (2))]]
let items = [[("orderId",Obj.repr (100));("sku",Obj.repr ("a"))]]
let result = (let __res0 = ref [] in
  List.iter (fun o ->
      List.iter (fun c ->
      let matched = ref false in
      List.iter (fun i ->
        if (Obj.obj (List.assoc "customerId" o) = Obj.obj (List.assoc "id" c)) && (Obj.obj (List.assoc "id" o) = Obj.obj (List.assoc "orderId" i)) then (
          __res0 := [("orderId",Obj.repr (Obj.obj (List.assoc "id" o)));("name",Obj.repr (Obj.obj (List.assoc "name" c)));("item",Obj.repr (i))] :: !__res0;
          matched := true)
      ) items;
      if not !matched then (
        let i = Obj.magic () in
        if (Obj.obj (List.assoc "customerId" o) = Obj.obj (List.assoc "id" c)) then __res0 := [("orderId",Obj.repr (Obj.obj (List.assoc "id" o)));("name",Obj.repr (Obj.obj (List.assoc "name" c)));("item",Obj.repr (i))] :: !__res0;
      );
      ) customers;
  ) orders;
List.rev !__res0)


let () =
  print_endline (__show ("--- Left Join Multi ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | r::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "orderId" r)) ^ " " ^ __show (Obj.obj (List.assoc "name" r)) ^ " " ^ __show (Obj.obj (List.assoc "item" r)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 result with Break -> ()
