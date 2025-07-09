open System

exception Break
exception Continue

printfn "%A" ("mochi".Substring(1, 4 - 1))
