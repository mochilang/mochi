open System

let m: System.Collections.Generic.IDictionary<string, string> = dict [(1, "a"); (2, "b")]
printfn "%b" (m.ContainsKey 1)
printfn "%b" (m.ContainsKey 3)
