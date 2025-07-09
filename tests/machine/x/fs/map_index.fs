open System

exception Break
exception Continue

let m = dict [("a", 1); ("b", 2)]
printfn "%A" (m.["b"])
