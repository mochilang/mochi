let m = [ (1, "a"); (2, "b") ]

let () =
  Printf.printf "%b\n" (List.assoc_opt 1 m <> None);
  Printf.printf "%b\n" (List.assoc_opt 3 m <> None)
