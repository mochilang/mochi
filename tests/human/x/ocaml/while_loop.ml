let rec loop i =
  if i < 3 then (
    print_endline (string_of_int i);
    loop (i + 1)
  )
let () = loop 0
