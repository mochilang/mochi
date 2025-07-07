let m = [ (1, "a"); (2, "b") ]

let () =
  match List.assoc_opt 1 m with
  | Some v -> print_endline v
  | None -> ()
