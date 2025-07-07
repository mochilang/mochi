let people = [
  ("Alice", 30);
  ("Bob", 15);
  ("Charlie", 65);
  ("Diana", 45)
]

let adults =
  List.map (fun (name,age) -> (name, age, age >= 60))
    (List.filter (fun (_,age) -> age >= 18) people)

let () =
  print_endline "--- Adults ---";
  List.iter (fun (name,age,senior) ->
    Printf.printf "%s is %d%s\n" name age (if senior then " (senior)" else "")) adults
