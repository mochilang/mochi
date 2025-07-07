let mutable matrix = [| [|1;2|]; [|3;4|] |]
matrix.[1].[0] <- 5
printfn "%d" matrix.[1].[0]
