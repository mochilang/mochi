// Generated 2025-07-25 00:53 +0700

exception Return

let PI: float = 3.141592653589793
let rec sinApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable n: int = 1
        while n <= 10 do
            let denom = float ((2 * n) * ((2 * n) + 1))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and cosApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n <= 10 do
            let denom = float (((2 * n) - 1) * (2 * n))
            term <- (((-term) * x) * x) / denom
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let degreesIncr: float = (0.1 * PI) / 180.0
let turns: float = 2.0
let stop: float = ((360.0 * turns) * 10.0) * degreesIncr
let width: float = 600.0
let centre: float = width / 2.0
let a: float = 1.0
let b: float = 20.0
let mutable theta: float = 0.0
let mutable count: int = 0
while theta < stop do
    let r: float = a + (b * theta)
    let x = r * (cosApprox theta)
    let y = r * (sinApprox theta)
    if (count % 100) = 0 then
        printfn "%s" (((string (centre + x)) + ",") + (string (centre - y)))
    theta <- theta + degreesIncr
    count <- count + 1
