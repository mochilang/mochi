open System

let m = dict [("a", 1); ("b", 2)]
printfn "%s" m.ContainsKey "a"
printfn "%s" m.ContainsKey "c"
