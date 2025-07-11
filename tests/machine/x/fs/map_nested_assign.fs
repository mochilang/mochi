open System

let mutable data: System.Collections.Generic.IDictionary<string, obj> = dict [("outer", dict [("inner", 1)])]
data.["outer"].["inner"] <- 2
printfn "%A" (data.["outer"].["inner"])
