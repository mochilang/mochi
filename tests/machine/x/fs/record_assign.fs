open System

exception Break
exception Continue

type Counter = {
    n: int
}
let inc (c) =
    c.n <- c.n + 1
let mutable c = { n = 0 }
printfn "%A" (inc c)
printfn "%A" (c.n)
