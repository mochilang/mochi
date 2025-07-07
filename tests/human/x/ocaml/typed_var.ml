let x : int option ref = ref None
let () = match !x with
  | Some v -> print_endline (string_of_int v)
  | None -> print_endline "nil"
