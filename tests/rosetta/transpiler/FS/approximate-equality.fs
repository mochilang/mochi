// Generated 2025-07-26 05:05 +0700

exception Return

let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and maxf (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- if a > b then a else b
        raise Return
        __ret
    with
        | Return -> __ret
and isClose (a: float) (b: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    try
        let relTol: float = 0.000000001
        let t: float = abs (a - b)
        let u = relTol * (float (maxf (float (abs a)) (float (abs b))))
        __ret <- t <= (float u)
        raise Return
        __ret
    with
        | Return -> __ret
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 10 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let root2: float = sqrtApprox 2.0
        let pairs: float array array = [|[|100000000000000.02; 100000000000000.02|]; [|100.01; 100.011|]; [|10000000000000.002 / 10000.0; 1000000000.0000001|]; [|0.001; 0.0010000001|]; [|0.000000000000000000000101; 0.0|]; [|root2 * root2; 2.0|]; [|(-root2) * root2; -2.0|]; [|100000000000000000.0; 100000000000000000.0|]; [|3.141592653589793; 3.141592653589793|]|]
        for pair in pairs do
            let a = pair.[0]
            let b = pair.[1]
            let s: string = if isClose (float a) (float b) then "≈" else "≉"
            printfn "%s" (((((string a) + " ") + s) + " ") + (string b))
        __ret
    with
        | Return -> __ret
main()
