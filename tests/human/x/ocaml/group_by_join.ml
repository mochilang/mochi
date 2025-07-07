let customers = [
  (1, "Alice");
  (2, "Bob")
]

let orders = [
  (100, 1);
  (101, 1);
  (102, 2)
]

let add_count map key =
  let count = try List.assoc key map with Not_found -> 0 in
  (key, count + 1) :: List.remove_assoc key map

let stats =
  List.fold_left (fun m (_, cid) ->
    match List.assoc_opt cid customers with
    | Some name -> add_count m name
    | None -> m)
    [] orders

let () =
  print_endline "--- Orders per customer ---";
  List.iter (fun (name, count) ->
    Printf.printf "%s orders: %d\n" name count) stats
