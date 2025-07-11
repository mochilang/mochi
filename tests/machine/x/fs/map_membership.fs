open System

let m: System.Collections.Generic.IDictionary<string, int> = dict [("a", 1); ("b", 2)]
printfn "%s" (m.ContainsKey "a")
printfn "%s" (m.ContainsKey "c")
