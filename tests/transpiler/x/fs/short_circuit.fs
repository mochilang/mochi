// Mochi 0.10.31 - generated 2025-07-19 12:44:58 UTC
open System

let boom a b =
    printfn "%s" (string "boom")
    true
printfn "%s" (string (false && (boom 1 2)))
printfn "%s" (string (true || (boom 1 2)))
