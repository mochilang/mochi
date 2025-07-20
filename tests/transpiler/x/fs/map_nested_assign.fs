// Generated 2025-07-20 13:26 +0700
open System

let mutable data = Map.ofList [("outer", Map.ofList [("inner", 1)])]
data <- Map.add "outer" (Map.add "inner" 2 (data.["outer"])) data
printfn "%s" (string (data.["outer"].["inner"]))
