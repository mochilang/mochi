open System

let mutable m: System.Collections.Generic.IDictionary<string, int> = dict [("a", 1); ("b", 2)]
for k in m do
    printfn "%A" (k)
