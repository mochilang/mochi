let m = [ ("a",1); ("b",2); ("c",3) ]
let values = List.map snd m
let () =
  List.iter (fun v -> Printf.printf "%d " v) values;
  print_newline ()
