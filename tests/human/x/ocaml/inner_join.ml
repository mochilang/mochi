let customers = [
  (1, "Alice");
  (2, "Bob");
  (3, "Charlie")
]

let orders = [
  (100, 1, 250);
  (101, 2, 125);
  (102, 1, 300);
  (103, 4, 80)
]

let join_results =
  List.filter_map (fun (oid, cid, total) ->
    try
      let name = List.assoc cid customers in
      Some (oid, name, total)
    with Not_found -> None)
    orders

let () =
  print_endline "--- Orders with customer info ---";
  List.iter (fun (oid, name, total) ->
    Printf.printf "Order %d by %s - $%d\n" oid name total)
    join_results
