let items = [
  ("a", 3);
  ("a", 1);
  ("b", 5);
  ("b", 2);
]

let add map (cat,value) =
  let sum = try List.assoc cat map with Not_found -> 0 in
  (cat, sum + value) :: List.remove_assoc cat map

let grouped = List.fold_left add [] items

let sorted = List.sort (fun (_,s1) (_,s2) -> compare s2 s1) grouped

let () =
  List.iter (fun (cat,total) ->
    Printf.printf "{cat=%S; total=%d}\n" cat total
  ) sorted
