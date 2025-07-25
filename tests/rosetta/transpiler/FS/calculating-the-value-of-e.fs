// Generated 2025-07-26 04:38 +0700

exception Break
exception Continue

exception Return

let epsilon: float = 0.000000000000001
let rec absf (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
and pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable r: float = 1.0
        let mutable i: int = 0
        while i < n do
            r <- r * 10.0
            i <- i + 1
        __ret <- r
        raise Return
        __ret
    with
        | Return -> __ret
and formatFloat (f: float) (prec: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable f = f
    let mutable prec = prec
    try
        let scale: float = pow10 prec
        let scaled: float = (f * scale) + 0.5
        let mutable n: int = int scaled
        let mutable digits: string = string n
        while (String.length digits) <= prec do
            digits <- "0" + digits
        let intPart: string = digits.Substring(0, ((String.length digits) - prec) - 0)
        let fracPart: string = digits.Substring((String.length digits) - prec, (String.length digits) - ((String.length digits) - prec))
        __ret <- (intPart + ".") + fracPart
        raise Return
        __ret
    with
        | Return -> __ret
let mutable factval: int = 1
let mutable e: float = 2.0
let mutable n: int = 2
let mutable term: float = 1.0
try
    while true do
        factval <- factval * n
        n <- n + 1
        term <- 1.0 / (float factval)
        e <- e + term
        if (float (absf term)) < epsilon then
            raise Break
with
| Break -> ()
| Continue -> ()
printfn "%s" ("e = " + (unbox<string> (formatFloat e 15)))
