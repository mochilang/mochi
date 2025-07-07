let xs = [1; 2; 3]

let () =
  Printf.printf "%b\n" (List.mem 2 xs);
  Printf.printf "%b\n" (not (List.mem 5 xs))
