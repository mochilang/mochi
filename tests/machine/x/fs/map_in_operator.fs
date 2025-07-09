open System

exception Break
exception Continue

let m = dict [(1, "a"); (2, "b")]
printfn "%b" (m.ContainsKey 1)
printfn "%b" (m.ContainsKey 3)
