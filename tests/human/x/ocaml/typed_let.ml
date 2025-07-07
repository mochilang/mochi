let y : int option = None
let () = match y with
  | Some v -> print_endline (string_of_int v)
  | None -> print_endline "nil"
