// Generated 2025-07-24 20:52 +0700

exception Return

let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 12 do
            let denom = float ((2 * n) * ((2 * n) + 1))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let dt: float = 0.01
let mutable s: float = 0.0
let mutable t1: float = 0.0
let mutable k1 = sinApprox 0.0
let mutable i: int = 1
while i <= 200 do
    let t2 = (float i) * dt
    let k2 = sinApprox (t2 * PI)
    s <- s + (((k1 + k2) * 0.5) * (t2 - t1))
    t1 <- t2
    k1 <- k2
    i <- i + 1
let mutable i2: int = 1
while i2 <= 50 do
    let t2 = 2.0 + ((float i2) * dt)
    let k2: float = 0.0
    s <- s + (((k1 + k2) * 0.5) * (t2 - t1))
    t1 <- t2
    k1 <- k2
    i2 <- i2 + 1
printfn "%.1f" s
