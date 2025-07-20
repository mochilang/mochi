// Generated 2025-07-20 14:31 +0700
open System

let m = Map.ofList [(1, "a"); (2, "b")]
printfn "%b" (Map.containsKey 1 m)
printfn "%b" (Map.containsKey 3 m)
