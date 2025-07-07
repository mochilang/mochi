let rec sum_rec n acc =
    if n = 0 then acc
    else sum_rec (n - 1) (acc + n)
printfn "%d" (sum_rec 10 0)
