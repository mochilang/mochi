open System

exception Break
exception Continue

let mutable data = dict [("outer", dict [("inner", 1)])]
data.["outer"].["inner"] <- 2
printfn "%A" (data.["outer"].["inner"])
