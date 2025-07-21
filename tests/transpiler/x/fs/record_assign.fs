// Generated 2025-07-21 13:50 +0700
open System

type Counter = {
    mutable n: int
}
let rec inc c =
    { c with n = (c.n) + 1 }
let mutable c: Counter = { n = 0 }
inc c
printfn "%s" (string (c.n))
