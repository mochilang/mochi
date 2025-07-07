open System

let sum_rec (n) (acc) =
    if n = 0 then
        acc
    sum_rec n - 1 acc + n
printfn "%A" (sum_rec 10 0)
