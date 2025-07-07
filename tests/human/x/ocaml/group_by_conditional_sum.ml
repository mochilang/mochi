
let items = [
  ("a", 10, true);
  ("a", 5, false);
  ("b", 20, true);
]

let add map (cat, value, flag) =
  let (sum_true,sum_all) = try List.assoc cat map with Not_found -> (0,0) in
  (cat, (sum_true + (if flag then value else 0), sum_all + value)) :: List.remove_assoc cat map

let grouped = List.fold_left add [] items

let sorted = List.sort (fun (c1,_) (c2,_) -> compare c1 c2) grouped

let () =
  List.iter (fun (cat,(t,a)) ->
    let share = float_of_int t /. float_of_int a in
    Printf.printf "{cat=%S; share=%g}\n" cat share
  ) sorted

