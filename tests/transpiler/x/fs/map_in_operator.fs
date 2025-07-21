// Generated 2025-07-21 15:37 +0700
open System

let m = Map.ofList [(1, "a"); (2, "b")]
printfn "%b" (Map.containsKey 1 m)
printfn "%b" (Map.containsKey 3 m)
