// Generated 2025-07-21 11:54 +0700
open System

let m = Map.ofList [("a", 1); ("b", 2); ("c", 3)]
printfn "%s" (string (List.map snd (Map.toList m)))
