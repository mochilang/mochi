let boom a b =
  print_endline "boom";
  true

let () =
  Printf.printf "%b\n" (false && boom 1 2);
  Printf.printf "%b\n" (true || boom 1 2)
