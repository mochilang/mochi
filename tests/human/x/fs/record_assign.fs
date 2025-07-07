type Counter = { mutable n: int }
let inc (c: Counter) = c.n <- c.n + 1
let c = { n = 0 }
inc c
printfn "%d" c.n
