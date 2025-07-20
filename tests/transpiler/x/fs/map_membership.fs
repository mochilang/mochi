// Generated 2025-07-20 16:45 +0700
open System

let m = Map.ofList [("a", 1); ("b", 2)]
printfn "%b" (Map.containsKey "a" m)
printfn "%b" (Map.containsKey "c" m)
