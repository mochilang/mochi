// Generated by Mochi compiler v0.10.27 on 2025-07-17T07:48:26Z
open System

let boom (a) (b) =
    printfn "%s" "boom"
    true
printfn "%b" (false && boom 1 2)
printfn "%b" (true || boom 1 2)
