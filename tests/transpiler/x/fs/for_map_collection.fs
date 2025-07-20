// Generated 2025-07-20 14:31 +0700
open System

let mutable m = Map.ofList [("a", 1); ("b", 2)]
for k in m do
printfn "%s" (string k)
