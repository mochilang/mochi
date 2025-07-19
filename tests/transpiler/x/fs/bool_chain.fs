// Mochi 0.10.31 - generated 2025-07-19 14:21:41 UTC
open System

let rec boom =
    printfn "%s" (string "boom")
    true
printfn "%s" (string (((1 < 2) && (2 < 3)) && (3 < 4)))
printfn "%s" (string (((1 < 2) && (2 > 3)) && (boom)))
printfn "%s" (string ((((1 < 2) && (2 < 3)) && (3 > 4)) && (boom)))
