let rec sum_rec n acc =
  if n = 0 then acc else sum_rec (n-1) (acc + n)

let () = print_endline (string_of_int (sum_rec 10 0))
