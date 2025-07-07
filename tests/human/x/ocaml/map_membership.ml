let m = [ ("a",1); ("b",2) ]
let in_map k = List.mem_assoc k m
let () =
  print_endline (string_of_bool (in_map "a"));
  print_endline (string_of_bool (in_map "c"))
