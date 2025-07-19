// Mochi 0.10.31 - generated 2025-07-19 14:22:49 UTC
open System

let rec boom a b =
    printfn "%s" (string "boom")
    true
printfn "%s" (string (false && (boom 1 2)))
printfn "%s" (string (true || (boom 1 2)))
