// Generated 2025-07-21 15:37 +0700
open System

type Anon1 = {
    mutable a: int
    mutable b: int
}
let mutable m: Anon1 = { a = 1; b = 2 }
for k in m do
printfn "%s" (string k)
