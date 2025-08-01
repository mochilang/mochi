// Generated 2025-08-01 18:27 +0700

exception Return

let _substring (s:string) (start:int) (finish:int) =
    let len = String.length s
    let mutable st = if start < 0 then len + start else start
    let mutable en = if finish < 0 then len + finish else finish
    if st < 0 then st <- 0
    if st > len then st <- len
    if en > len then en <- len
    if st > en then st <- en
    s.Substring(st, en - st)

let rec pow10 (n: int) =
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
        let intPart: string = _substring digits 0 ((String.length digits) - prec)
        let fracPart: string = _substring digits ((String.length digits) - prec) (String.length digits)
        __ret <- (intPart + ".") + fracPart
        raise Return
        __ret
    with
        | Return -> __ret
and padLeftZeros (s: string) (width: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable width = width
    try
        let mutable out: string = s
        while (String.length out) < width do
            out <- "0" + out
        __ret <- out
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (padLeftZeros (formatFloat 7.125 3) 9)
