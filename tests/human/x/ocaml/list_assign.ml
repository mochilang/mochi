let nums = [|1;2|]
let () =
  nums.(1) <- 3;
  print_endline (string_of_int nums.(1))
