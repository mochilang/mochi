// Generated 2025-07-20 10:18 +0700
open System

let rec boom a b =
    printfn "%s" (string "boom")
    true
printfn "%b" (false && (boom 1 2))
printfn "%b" (true || (boom 1 2))
