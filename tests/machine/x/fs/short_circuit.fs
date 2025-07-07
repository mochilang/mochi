open System

let boom (a) (b) =
    printfn "%A" ("boom")
    true
printfn "%A" (false && boom 1 2)
printfn "%A" (true || boom 1 2)
