// Generated 2025-07-22 09:07 +0700

let mutable m = Map.ofList [("a", 1); ("b", 2)]
for KeyValue(k, _) in m do
printfn "%s" (string k)
