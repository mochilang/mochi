// Generated 2025-07-22 09:07 +0700

let mutable x: int = 3
let mutable y: int = 4
let mutable m = Map.ofList [("a", x); ("b", y)]
printfn "%s" (String.concat " " [string (m.["a"]); string (m.["b"])])
