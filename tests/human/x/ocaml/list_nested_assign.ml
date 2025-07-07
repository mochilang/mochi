let matrix = [| [|1;2|]; [|3;4|] |]
let () =
  matrix.(1).(0) <- 5;
  print_endline (string_of_int matrix.(1).(0))
