let customers = [
  (1, "Alice");
  (2, "Bob")
]

let orders = [
  (100, 1);
  (101, 2)
]

let items = [
  (100, "a");
  (101, "b")
]

let items_for_order oid =
  List.filter (fun (oid', _) -> oid' = oid) items

let result =
  List.fold_left (fun acc (oid, cid) ->
    match List.assoc_opt cid customers with
    | None -> acc
    | Some name ->
        let lst = items_for_order oid in
        List.fold_left (fun acc (_, sku) -> (name, sku) :: acc) acc lst
  ) [] orders
  |> List.rev

let () =
  print_endline "--- Multi Join ---";
  List.iter (fun (name, sku) ->
    Printf.printf "%s bought item %s\n" name sku) result
