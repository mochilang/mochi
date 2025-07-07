let people = [
  ("Alice", 30);
  ("Bob", 25);
]

let () =
  List.iter (fun (name, age) ->
    Printf.printf "{\"name\": \"%s\", \"age\": %d}\n" name age
  ) people
