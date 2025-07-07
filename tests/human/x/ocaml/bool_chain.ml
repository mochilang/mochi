let boom () =
  print_endline "boom";
  true

let () =
  print_endline (string_of_bool ((1 < 2) && (2 < 3) && (3 < 4)));
  print_endline (string_of_bool ((1 < 2) && (2 > 3) && boom ()));
  print_endline (string_of_bool ((1 < 2) && (2 < 3) && (3 > 4) && boom ()))
