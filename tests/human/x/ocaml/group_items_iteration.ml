let data = [
  ("a", 1);
  ("a", 2);
  ("b", 3);
]

let add map (tag,value) =
  let lst = try List.assoc tag map with Not_found -> [] in
  (tag, value :: lst) :: List.remove_assoc tag map

let groups = List.fold_left add [] data

let totals =
  List.map (fun (tag,values) ->
    (tag, List.fold_left (+) 0 values)
  ) groups
  |> List.sort (fun (a,_) (b,_) -> compare a b)

let () =
  List.iter (fun (tag,total) ->
    Printf.printf "{tag=%S; total=%d}\n" tag total
  ) totals
