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


let products = [[("name",Obj.repr ("Laptop"));("price",Obj.repr (1500))];[("name",Obj.repr ("Smartphone"));("price",Obj.repr (900))];[("name",Obj.repr ("Tablet"));("price",Obj.repr (600))];[("name",Obj.repr ("Monitor"));("price",Obj.repr (300))];[("name",Obj.repr ("Keyboard"));("price",Obj.repr (100))];[("name",Obj.repr ("Mouse"));("price",Obj.repr (50))];[("name",Obj.repr ("Headphones"));("price",Obj.repr (200))]]
let expensive = (let __res0 = ref [] in
  List.iter (fun p ->
      __res0 := p :: !__res0;
  ) products;
List.rev !__res0)


let () =
  print_endline (__show ("--- Top products (excluding most expensive) ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | item::rest ->
        try
          print_endline (__show (Obj.obj (List.assoc "name" item)) ^ " " ^ __show ("costs $") ^ " " ^ __show (Obj.obj (List.assoc "price" item)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 expensive with Break -> ()
