// Generated 2025-07-21 15:37 +0700
open System

let xs: int list = [1; 2; 3]
printfn "%b" (List.contains 2 xs)
printfn "%b" (not (List.contains 5 xs))
