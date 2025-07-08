let matrix = ref [[1;2];[3;4]]

let () =
  matrix := 5
  print_endline List.nth List.nth !matrix 1 0
