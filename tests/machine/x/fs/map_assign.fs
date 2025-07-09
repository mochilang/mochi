open System

exception Break
exception Continue

let mutable scores = dict [("alice", 1)]
scores.["bob"] <- 2
printfn "%A" (scores.["bob"])
