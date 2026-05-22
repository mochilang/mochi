// Generated 2025-07-21 18:37 +0700

let mutable matrix: list list = [[1; 2]; [3; 4]]
matrix.[1].[0] <- 5
printfn "%s" (string (matrix.[1].[0]))
