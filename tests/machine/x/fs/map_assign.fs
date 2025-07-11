open System

let mutable scores: System.Collections.Generic.IDictionary<string, int> = dict [("alice", 1)]
scores.["bob"] <- 2
printfn "%A" (scores.["bob"])
