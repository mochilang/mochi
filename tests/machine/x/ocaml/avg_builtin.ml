
let () =
  print_endline (List.fold_left (+) 0 [1;2;3] / List.length [1;2;3])
