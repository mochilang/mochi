// Mochi 0.10.31 - generated 2025-07-19 12:44:51 UTC
open System

let outer x =
    let inner y =
    x + y
    inner 5
printfn "%s" (string (outer 3))
