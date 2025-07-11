open System

let mutable matrix: obj = [|[1; 2]; [3; 4]|]
matrix.[1].[0] <- 5
printfn "%A" (matrix.[1].[0])
