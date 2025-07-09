open System

exception Break
exception Continue

let outer (x) =
    let inner (y) =
        x + y
    inner 5
printfn "%A" (outer 3)
