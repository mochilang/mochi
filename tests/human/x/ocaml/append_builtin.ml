let a = [1;2]
let b = a @ [3]
let () =
  List.iter (fun i -> print_string (string_of_int i ^ " ")) b;
  print_newline ()
