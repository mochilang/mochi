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
  type record2 = { mutable id : int; mutable customerId : int }
  type record3 = { mutable orderId : int; mutable sku : string }
  type record4 = { mutable orderId : int; mutable name : string; mutable item : record3 }

let customers : record1 list = [{ id = 1; name = "Alice" };{ id = 2; name = "Bob" }]
let orders : record2 list = [{ id = 100; customerId = 1 };{ id = 101; customerId = 2 }]
let items : record3 list = [{ orderId = 100; sku = "a" }]
let result : record4 list = (let __res0 = ref [] in
  List.iter (fun (o : record2) ->
      List.iter (fun (c : record1) ->
      let matched = ref false in
      List.iter (fun i ->
        if (o.customerId = c.id) && (o.id = i.orderId) then (
          __res0 := { orderId = o.id; name = c.name; item = i } :: !__res0;
          matched := true)
      ) items;
      if not !matched then (
        let i = Obj.magic () in
        if (o.customerId = c.id) then __res0 := { orderId = o.id; name = c.name; item = i } :: !__res0;
      );
      ) customers;
  ) orders;
List.rev !__res0)


let () =
  print_endline "--- Left Join Multi ---";
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | (r : record4)::rest ->
        (try
          print_endline (__show (r.orderId) ^ " " ^ __show (r.name) ^ " " ^ __show (r.item));
        with Continue -> ())
        ; __loop1 rest
    in
    try __loop1 result with Break -> ()
