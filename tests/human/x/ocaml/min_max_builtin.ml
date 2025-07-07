let nums = [3;1;4]
let () =
  print_endline (string_of_int (List.fold_left min max_int nums));
  print_endline (string_of_int (List.fold_left max min_int nums))
