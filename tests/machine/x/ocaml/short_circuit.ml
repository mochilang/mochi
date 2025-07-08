let rec boom a b =
  print_endline "boom"
  true


let () =
  print_endline (false && boom 1 2)
  print_endline (true || boom 1 2)
