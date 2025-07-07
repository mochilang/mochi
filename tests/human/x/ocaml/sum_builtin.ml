let () =
  let nums = [1;2;3] in
  let s = List.fold_left (+) 0 nums in
  print_endline (string_of_int s)
