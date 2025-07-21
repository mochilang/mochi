// Generated 2025-07-21 15:37 +0700
open System

let mutable nums: int list = [1; 2]
nums.[1] <- 3
printfn "%s" (string (nums.[1]))
