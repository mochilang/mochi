let makeAdder n = fun x -> x + n
let add10 = makeAdder 10
let () = print_endline (string_of_int (add10 7))
