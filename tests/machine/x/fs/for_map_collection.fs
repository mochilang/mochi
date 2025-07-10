open System

let mutable m = dict [("a", 1); ("b", 2)]
for k in m do
    printfn "%A" (k)
