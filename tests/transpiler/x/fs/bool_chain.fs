// Generated 2025-07-21 15:37 +0700
open System

let rec boom () =
    printfn "%s" (string "boom")
    true
printfn "%b" (((1 < 2) && (2 < 3)) && (3 < 4))
printfn "%b" (((1 < 2) && (2 > 3)) && (boom()))
printfn "%b" ((((1 < 2) && (2 < 3)) && (3 > 4)) && (boom()))
