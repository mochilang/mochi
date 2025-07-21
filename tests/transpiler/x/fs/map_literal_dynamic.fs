// Generated 2025-07-21 15:37 +0700
open System

type Anon1 = {
    mutable a: int
    mutable b: int
}
let mutable x: int = 3
let mutable y: int = 4
let mutable m: Anon1 = { a = x; b = y }
printfn "%s" (String.concat " " [string (m.["a"]); string (m.["b"])])
