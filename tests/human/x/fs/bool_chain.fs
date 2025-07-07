open System

let boom () =
    printfn "boom"
    true

printfn "%b" ((1 < 2) && (2 < 3) && (3 < 4))
printfn "%b" ((1 < 2) && (2 > 3) && boom())
printfn "%b" ((1 < 2) && (2 < 3) && (3 > 4) && boom())
