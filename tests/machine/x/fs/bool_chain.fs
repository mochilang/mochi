open System

exception Break
exception Continue

let boom () =
    printfn "%s" "boom"
    true
printfn "%b" ((1 < 2) && (2 < 3) && (3 < 4))
printfn "%b" ((1 < 2) && (2 > 3) && boom())
printfn "%b" ((1 < 2) && (2 < 3) && (3 > 4) && boom())
