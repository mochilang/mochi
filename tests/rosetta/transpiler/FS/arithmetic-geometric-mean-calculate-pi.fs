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
and agmPi () =
    let mutable __ret : float = Unchecked.defaultof<float>
    try
        let mutable a: float = 1.0
        let mutable g = 1.0 / (sqrtApprox 2.0)
        let mutable sum: float = 0.0
        let mutable pow: float = 2.0
        while (abs (a - g)) > 0.000000000000001 do
            let mutable t = (a + g) / 2.0
            let mutable u = sqrtApprox (a * g)
            a <- t
            g <- u
            pow <- pow * 2.0
            let mutable diff = (a * a) - (g * g)
            sum <- sum + (diff * pow)
        let mutable pi: float = ((4.0 * a) * a) / (1.0 - sum)
        __ret <- pi
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        printfn "%s" (string (agmPi()))
        __ret
    with
        | Return -> __ret
main()
