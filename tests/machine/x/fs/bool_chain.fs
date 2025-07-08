open System

exception Break
exception Continue

let boom () =
    printfn "%s" "boom"
    true
printfn "%A" ((1 < 2) && (2 < 3) && (3 < 4))
printfn "%A" ((1 < 2) && (2 > 3) && boom())
printfn "%A" ((1 < 2) && (2 < 3) && (3 > 4) && boom())
