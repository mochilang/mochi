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


let customers = [[("id",Obj.repr (1));("name",Obj.repr ("Alice"))];[("id",Obj.repr (2));("name",Obj.repr ("Bob"))];[("id",Obj.repr (3));("name",Obj.repr ("Charlie"))];[("id",Obj.repr (4));("name",Obj.repr ("Diana"))]]
let orders = [[("id",Obj.repr (100));("customerId",Obj.repr (1));("total",Obj.repr (250))];[("id",Obj.repr (101));("customerId",Obj.repr (2));("total",Obj.repr (125))];[("id",Obj.repr (102));("customerId",Obj.repr (1));("total",Obj.repr (300))]]
let result = (let __res0 = ref [] in
  List.iter (fun o ->
    let matched = ref false in
    List.iter (fun c ->
      if (Obj.obj (List.assoc "customerId" o) = Obj.obj (List.assoc "id" c)) then (
        __res0 := [("customerName",Obj.repr (Obj.obj (List.assoc "name" c)));("order",Obj.repr (o))] :: !__res0;
        matched := true)
    ) customers;
    if not !matched then (
      let c = Obj.magic () in
      __res0 := [("customerName",Obj.repr (Obj.obj (List.assoc "name" c)));("order",Obj.repr (o))] :: !__res0;
    );
  ) orders;
  List.rev !__res0)


let () =
  print_endline (__show ("--- Right Join using syntax ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | entry::rest ->
        try
          if Obj.obj (List.assoc "order" entry) then (
            print_endline (__show ("Customer") ^ " " ^ __show (Obj.obj (List.assoc "customerName" entry)) ^ " " ^ __show ("has order") ^ " " ^ __show (Obj.obj (List.assoc "order" entry).id) ^ " " ^ __show ("- $") ^ " " ^ __show (Obj.obj (List.assoc "order" entry).total));
          ) else (
            print_endline (__show ("Customer") ^ " " ^ __show (Obj.obj (List.assoc "customerName" entry)) ^ " " ^ __show ("has no orders"));
          ) ;
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 result with Break -> ()
