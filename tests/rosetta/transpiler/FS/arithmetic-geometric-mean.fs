// Generated 2025-07-25 01:11 +0700

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
and sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = x
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
and agm (a: float) (g: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable g = g
    try
        let eps: float = 0.00000000000001
        while (abs (a - g)) > ((abs a) * eps) do
            let newA: float = (a + g) / 2.0
            let newG = sqrtApprox (a * g)
            a <- newA
            g <- newG
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" (string (agm 1.0 (1.0 / (sqrtApprox 2.0))))
        __ret
    with
        | Return -> __ret
main()
