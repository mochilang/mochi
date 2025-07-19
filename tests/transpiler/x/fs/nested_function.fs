// Mochi 0.10.31 - generated 2025-07-19 13:09:14 UTC
open System

let rec outer x =
    let rec inner y =
    x + y
    inner 5
printfn "%s" (string (outer 3))
