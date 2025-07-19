// Mochi 0.10.31 - generated 2025-07-19 12:44:36 UTC
open System

let x = 8
let msg = if x > 10 then "big" else (if x > 5 then "medium" else "small")
printfn "%s" (string msg)
