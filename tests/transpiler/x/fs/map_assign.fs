// Generated 2025-07-22 09:07 +0700

let mutable scores = Map.ofList [("alice", 1)]
scores <- Map.add "bob" 2 scores
printfn "%s" (string (scores.["bob"]))
