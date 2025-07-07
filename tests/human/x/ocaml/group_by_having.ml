let people = [
  ("Alice", "Paris");
  ("Bob", "Hanoi");
  ("Charlie", "Paris");
  ("Diana", "Hanoi");
  ("Eve", "Paris");
  ("Frank", "Hanoi");
  ("George", "Paris")
]

let add map city =
  let count = try List.assoc city map with Not_found -> 0 in
  (city, count + 1) :: List.remove_assoc city map

let grouped =
  List.fold_left (fun m (_,city) -> add m city) [] people

let big = List.filter (fun (_,count) -> count >= 4) grouped

let json_of_groups lst =
  "[" ^
  String.concat ", " (
    List.map (fun (city,count) ->
      Printf.sprintf "{\"city\":\"%s\",\"num\":%d}" city count
    ) lst)
  ^ "]"

let () =
  print_endline (json_of_groups big)
