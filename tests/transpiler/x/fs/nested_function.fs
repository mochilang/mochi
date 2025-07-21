// Generated 2025-07-21 15:37 +0700
open System

let rec outer x =
    let rec inner y =
    x + y
    inner 5
printfn "%s" (string (outer 3))
