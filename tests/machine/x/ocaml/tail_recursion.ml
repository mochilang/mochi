let rec sum_rec n acc =
  if (n = 0) then (
    acc
  )
  sum_rec (n - 1) (acc + n)


let () =
  print_endline sum_rec 10 0
