// Mochi 0.10.31 - generated 2025-07-19 14:22:36 UTC
open System

let rec outer x =
    let rec inner y =
    x + y
    inner 5
printfn "%s" (string (outer 3))
