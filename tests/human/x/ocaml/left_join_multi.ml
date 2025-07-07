let customers = [
  (1, "Alice");
  (2, "Bob")
]

let orders = [
  (100, 1);
  (101, 2)
]

let items = [
  (100, "a")
]

let item_for_order oid =
  try Some (List.assoc oid items) with Not_found -> None

let result =
  List.map (fun (oid, cid) ->
    let name = List.assoc cid customers in
    (oid, name, item_for_order oid)) orders

let () =
  print_endline "--- Left Join Multi ---";
  List.iter (fun (oid, name, item) ->
    match item with
    | Some sku -> Printf.printf "%d %s %s\n" oid name sku
    | None -> Printf.printf "%d %s None\n" oid name)
    result
