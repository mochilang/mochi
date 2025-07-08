
let () =
  print_endline ([1;2] union [2;3])
  print_endline ([1;2;3] except [2])
  print_endline ([1;2;3] intersect [2;4])
  print_endline List.length ([1;2] union [2;3])
