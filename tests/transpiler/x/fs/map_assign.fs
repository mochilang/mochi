// Generated 2025-07-20 13:26 +0700
open System

let mutable scores = Map.ofList [("alice", 1)]
scores <- Map.add "bob" 2 scores
printfn "%s" (string (scores.["bob"]))
