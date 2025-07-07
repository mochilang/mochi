let customers = [
  (1, "Alice");
  (2, "Bob");
  (3, "Charlie");
  (4, "Diana")
]

let orders = [
  (100, 1, 250);
  (101, 2, 125);
  (102, 1, 300);
  (103, 5, 80)
]

let customer_of cid =
  try Some (List.assoc cid customers) with Not_found -> None

let order_customer_ids = List.map (fun (_, cid, _) -> cid) orders

let () =
  print_endline "--- Outer Join using syntax ---";
  List.iter (fun (oid, cid, total) ->
    match customer_of cid with
    | Some name -> Printf.printf "Order %d by %s - $%d\n" oid name total
    | None -> Printf.printf "Order %d by Unknown - $%d\n" oid total)
    orders;
  List.iter (fun (cid, name) ->
    if not (List.mem cid order_customer_ids) then
      Printf.printf "Customer %s has no orders\n" name)
    customers
