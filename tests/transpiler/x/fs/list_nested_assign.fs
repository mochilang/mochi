// Generated 2025-07-21 15:37 +0700
open System

let mutable matrix: list list = [[1; 2]; [3; 4]]
matrix.[1].[0] <- 5
printfn "%s" (string (matrix.[1].[0]))
