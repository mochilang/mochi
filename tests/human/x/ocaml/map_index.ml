let m = [ ("a",1); ("b",2) ]
let () = print_endline (string_of_int (List.assoc "b" m))
