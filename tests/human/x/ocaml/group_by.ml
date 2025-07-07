let people = [
  ("Alice", 30, "Paris");
  ("Bob", 15, "Hanoi");
  ("Charlie", 65, "Paris");
  ("Diana", 45, "Hanoi");
  ("Eve", 70, "Paris");
  ("Frank", 22, "Hanoi")
]

let add map city age =
  let (count,sum) = try List.assoc city map with Not_found -> (0,0) in
  (city, (count+1, sum+age)) :: List.remove_assoc city map

let stats =
  let grouped = List.fold_left (fun m (name,age,city) -> add m city age) [] people in
  List.map (fun (city,(count,sum)) -> (city,count, float_of_int sum /. float_of_int count)) grouped

let () =
  print_endline "--- People grouped by city ---";
  List.iter (fun (city,count,avg_age) ->
    Printf.printf "%s : count = %d , avg_age = %g\n" city count avg_age) stats
