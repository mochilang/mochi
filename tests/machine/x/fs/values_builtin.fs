open System

let m: System.Collections.Generic.IDictionary<string, int> = dict [("a", 1); ("b", 2); ("c", 3)]
printfn "%s" (String.concat " " (List.map string (Seq.toList (m.Values))))
