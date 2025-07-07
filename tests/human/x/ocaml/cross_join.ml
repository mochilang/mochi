let customers = [
  (1, "Alice");
  (2, "Bob");
  (3, "Charlie")
]

let orders = [
  (100, 1, 250);
  (101, 2, 125);
  (102, 1, 300)
]

let result =
  List.concat (List.map (fun (oid,cid,total) ->
    List.map (fun (_,name) -> (oid,cid,total,name)) customers
  ) orders)

let () =
  print_endline "--- Cross Join: All order-customer pairs ---";
  List.iter (fun (oid,cid,total,name) ->
    Printf.printf "Order %d (customerId: %d , total: $ %d ) paired with %s\n" oid cid total name)
    result
