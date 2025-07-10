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


  type record1 = { mutable id : int; mutable name : string }
  type record2 = { mutable id : int; mutable customerId : int; mutable total : int }
  type record3 = { mutable orderId : int; mutable customerName : Obj.t; mutable total : int }

let customers : record1 list = [{ id = 1; name = "Alice" };{ id = 2; name = "Bob" };{ id = 3; name = "Charlie" }]
let orders : record2 list = [{ id = 100; customerId = 1; total = 250 };{ id = 101; customerId = 2; total = 125 };{ id = 102; customerId = 1; total = 300 };{ id = 103; customerId = 4; total = 80 }]
let result : (string * Obj.t) list list = (let __res0 = ref [] in
  List.iter (fun o ->
    List.iter (fun c ->
      if (Obj.obj (List.assoc "customerId" o) = Obj.obj (List.assoc "id" c)) then (
        __res0 := { orderId = Obj.obj (List.assoc "id" o); customerName = Obj.obj (List.assoc "name" c); total = Obj.obj (List.assoc "total" o) } :: !__res0;
      )
    ) customers;
  ) orders;
  List.rev !__res0)


let () =
  print_endline (__show ("--- Orders with customer info ---"));
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | entry::rest ->
        try
          print_endline (__show ("Order") ^ " " ^ __show (Obj.obj (List.assoc "orderId" entry)) ^ " " ^ __show ("by") ^ " " ^ __show (Obj.obj (List.assoc "customerName" entry)) ^ " " ^ __show ("- $") ^ " " ^ __show (Obj.obj (List.assoc "total" entry)));
        with Continue -> ()
        ; __loop1 rest
    in
    try __loop1 result with Break -> ()
