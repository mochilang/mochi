let data = [1;2]
let flag = List.exists (fun x -> x = 1) data
let () = print_endline (string_of_bool flag)
