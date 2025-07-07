let customers = [
  (1, "Alice");
  (2, "Bob");
  (3, "Charlie")
]

let orders = [
  (100, 1);
  (101, 1);
  (102, 2)
]

let initial = List.map (fun (_, name) -> (name, 0)) customers

let add_count map key =
  let count = List.assoc key map in
  (key, count + 1) :: List.remove_assoc key map

let stats =
  List.fold_left (fun m (_, cid) ->
    match List.assoc_opt cid customers with
    | Some name -> add_count m name
    | None -> m)
    initial orders

let () =
  print_endline "--- Group Left Join ---";
  List.iter (fun (name, count) ->
    Printf.printf "%s orders: %d\n" name count) stats
