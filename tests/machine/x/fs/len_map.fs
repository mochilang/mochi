open System

exception Break
exception Continue

printfn "%A" (List.length dict [("a", 1); ("b", 2)])
