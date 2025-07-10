open System

let mutable nums = [|1; 2|]
nums.[1] <- 3
printfn "%A" (nums.[1])
