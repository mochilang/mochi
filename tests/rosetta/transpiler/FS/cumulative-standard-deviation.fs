// Generated 2025-07-28 11:14 +0700

exception Return

type Rsdv = {
    n: float
    a: float
    q: float
}
let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable g: float = x
        let mutable i: int = 0
        while i < 20 do
            g <- (g + (x / g)) / 2.0
            i <- i + 1
        __ret <- g
        raise Return
        __ret
    with
        | Return -> __ret
and newRsdv () =
    let mutable __ret : Rsdv = Unchecked.defaultof<Rsdv>
    try
        __ret <- { n = 0.0; a = 0.0; q = 0.0 }
        raise Return
        __ret
    with
        | Return -> __ret
and add (r: Rsdv) (x: float) =
    let mutable __ret : Rsdv = Unchecked.defaultof<Rsdv>
    let mutable r = r
    let mutable x = x
    try
        let n1: float = (r.n) + 1.0
        let a1: float = (r.a) + ((x - (r.a)) / n1)
        let q1: float = (r.q) + ((x - (r.a)) * (x - a1))
        __ret <- { n = n1; a = a1; q = q1 }
        raise Return
        __ret
    with
        | Return -> __ret
and sd (r: Rsdv) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable r = r
    try
        __ret <- sqrtApprox ((r.q) / (r.n))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable r: Rsdv = newRsdv()
        for x in [|2.0; 4.0; 4.0; 4.0; 5.0; 5.0; 7.0; 9.0|] do
            r <- add r (float x)
            printfn "%s" (string (sd r))
        __ret
    with
        | Return -> __ret
main()
