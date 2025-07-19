// Mochi 0.10.31 - generated 2025-07-19 12:44:19 UTC
open System

let boom =
    printfn "%s" (string "boom")
    true
printfn "%s" (string (((1 < 2) && (2 < 3)) && (3 < 4)))
printfn "%s" (string (((1 < 2) && (2 > 3)) && (boom)))
printfn "%s" (string ((((1 < 2) && (2 < 3)) && (3 > 4)) && (boom)))
