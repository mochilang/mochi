let avg lst =
  List.fold_left (+) 0 lst / List.length lst
let () = print_endline (string_of_int (avg [1;2;3]))
