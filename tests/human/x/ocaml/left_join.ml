let customers = [
  (1, "Alice");
  (2, "Bob")
]

let orders = [
  (100, 1, 250);
  (101, 3, 80)
]

let lookup_customer cid =
  try Some (List.assoc cid customers) with Not_found -> None

let result =
  List.map (fun (oid, cid, total) ->
    (oid, lookup_customer cid, total)) orders

let () =
  print_endline "--- Left Join ---";
  List.iter (fun (oid, customer, total) ->
    match customer with
    | Some name -> Printf.printf "Order %d customer %s total %d\n" oid name total
    | None -> Printf.printf "Order %d customer None total %d\n" oid total)
    result
