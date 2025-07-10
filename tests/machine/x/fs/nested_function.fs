open System

let outer (x) =
    let inner (y) =
        x + y
    inner 5
printfn "%A" (outer 3)
