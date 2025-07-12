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
  type record3 = { mutable orderId : int; mutable orderCustomerId : int; mutable pairedCustomerName : string; mutable orderTotal : int }

let customers : record1 list = [{ id = 1; name = "Alice" };{ id = 2; name = "Bob" };{ id = 3; name = "Charlie" }]
let orders : record2 list = [{ id = 100; customerId = 1; total = 250 };{ id = 101; customerId = 2; total = 125 };{ id = 102; customerId = 1; total = 300 }]
let result : record3 list = (let __res0 = ref [] in
  List.iter (fun (o : record2) ->
      List.iter (fun (c : record1) ->
              __res0 := { orderId = o.id; orderCustomerId = o.customerId; pairedCustomerName = c.name; orderTotal = o.total } :: !__res0;
      ) customers;
  ) orders;
List.rev !__res0)


let () =
  print_endline "--- Cross Join: All order-customer pairs ---";
  let rec __loop1 lst =
    match lst with
      | [] -> ()
      | (entry : record3)::rest ->
        (try
          print_endline (__show ("Order") ^ " " ^ __show (entry.orderId) ^ " " ^ __show ("(customerId:") ^ " " ^ __show (entry.orderCustomerId) ^ " " ^ __show (", total: $") ^ " " ^ __show (entry.orderTotal) ^ " " ^ __show (") paired with") ^ " " ^ __show (entry.pairedCustomerName));
        with Continue -> ())
        ; __loop1 rest
    in
    try __loop1 result with Break -> ()
