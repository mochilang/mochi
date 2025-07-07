let s = "catch"
let () =
  print_endline (string_of_bool (String.contains s 'c' && String.sub s 0 3 = "cat"));
  print_endline (string_of_bool (String.contains s 'd'))
