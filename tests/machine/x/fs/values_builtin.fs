open System

exception Break
exception Continue

let m = dict [("a", 1); ("b", 2); ("c", 3)]
printfn "%s" (String.concat " " (List.map string (Seq.toList (m.Values))))
