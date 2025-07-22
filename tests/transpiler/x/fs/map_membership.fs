// Generated 2025-07-22 09:07 +0700

let m = Map.ofList [("a", 1); ("b", 2)]
printfn "%d" (if Map.containsKey "a" m then 1 else 0)
printfn "%d" (if Map.containsKey "c" m then 1 else 0)
