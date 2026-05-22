// Generated 2025-07-21 18:37 +0700

let rec sum_rec n acc =
    if n = 0 then acc else (sum_rec (n - 1) (acc + n))
printfn "%s" (string (sum_rec 10 0))
