// Mochi 0.10.31 - generated 2025-07-19 13:09:37 UTC
open System

let rec sum_rec n acc =
    if n = 0 then
acc
    sum_rec (n - 1) (acc + n)
printfn "%s" (string (sum_rec 10 0))
