let customers = [
  (1, "Alice");
  (2, "Bob");
  (3, "Charlie");
  (4, "Diana")
]

let orders = [
  (100, 1, 250);
  (101, 2, 125);
  (102, 1, 300)
]

let orders_for_customer cid =
  List.filter (fun (_, c, _) -> c = cid) orders

let () =
  print_endline "--- Right Join using syntax ---";
  List.iter (fun (cid, name) ->
    let os = orders_for_customer cid in
    if os = [] then
      Printf.printf "Customer %s has no orders\n" name
    else
      List.iter (fun (oid, _, total) ->
        Printf.printf "Customer %s has order %d - $%d\n" name oid total) os
  ) customers
