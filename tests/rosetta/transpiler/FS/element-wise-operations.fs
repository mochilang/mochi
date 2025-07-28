// Generated 2025-07-28 10:03 +0700

exception Return

let mutable _nowSeed:int64 = 0L
let mutable _nowSeeded = false
let _initNow () =
    let s = System.Environment.GetEnvironmentVariable("MOCHI_NOW_SEED")
    if System.String.IsNullOrEmpty(s) |> not then
        match System.Int32.TryParse(s) with
        | true, v ->
            _nowSeed <- int64 v
            _nowSeeded <- true
        | _ -> ()
let _now () =
    if _nowSeeded then
        _nowSeed <- (_nowSeed * 1664525L + 1013904223L) % 2147483647L
        int _nowSeed
    else
        int (System.DateTime.UtcNow.Ticks % 2147483647L)

_initNow()
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
and powf (``base``: float) (exp: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        if exp = 0.5 then
            let mutable guess: float = ``base``
            let mutable i: int = 0
            while i < 20 do
                guess <- (guess + (``base`` / guess)) / 2.0
                i <- i + 1
            __ret <- guess
            raise Return
        let mutable result: float = 1.0
        let mutable n: int = int exp
        let mutable i: int = 0
        while i < n do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
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
and padLeft (s: string) (w: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable s = s
    let mutable w = w
    try
        let mutable res: string = ""
        let mutable n: int = w - (String.length s)
        while n > 0 do
            res <- res + " "
            n <- n - 1
        __ret <- res + s
        raise Return
        __ret
    with
        | Return -> __ret
and rowString (row: float array) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable row = row
    try
        let mutable s: string = "["
        let mutable i: int = 0
        while i < (Seq.length row) do
            s <- s + (unbox<string> (padLeft (formatFloat (row.[i]) 3) 6))
            if i < ((Seq.length row) - 1) then
                s <- s + " "
            i <- i + 1
        __ret <- s + "] "
        raise Return
        __ret
    with
        | Return -> __ret
and printMatrix (heading: string) (m: float array array) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable heading = heading
    let mutable m = m
    try
        printfn "%s" heading
        let mutable i: int = 0
        while i < (Seq.length m) do
            printfn "%s" (rowString (m.[i]))
            i <- i + 1
        __ret
    with
        | Return -> __ret
and elementWiseMM (m1: float array array) (m2: float array array) (f: float -> float -> float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m1 = m1
    let mutable m2 = m2
    let mutable f = f
    try
        let mutable z: float array array = [||]
        let mutable r: int = 0
        while r < (Seq.length m1) do
            let mutable row: float array = [||]
            let mutable c: int = 0
            while c < (Seq.length (m1.[r])) do
                row <- Array.append row [|unbox<float> (f ((m1.[r]).[c]) ((m2.[r]).[c]))|]
                c <- c + 1
            z <- Array.append z [|row|]
            r <- r + 1
        __ret <- z
        raise Return
        __ret
    with
        | Return -> __ret
and elementWiseMS (m: float array array) (s: float) (f: float -> float -> float) =
    let mutable __ret : float array array = Unchecked.defaultof<float array array>
    let mutable m = m
    let mutable s = s
    let mutable f = f
    try
        let mutable z: float array array = [||]
        let mutable r: int = 0
        while r < (Seq.length m) do
            let mutable row: float array = [||]
            let mutable c: int = 0
            while c < (Seq.length (m.[r])) do
                row <- Array.append row [|unbox<float> (f ((m.[r]).[c]) s)|]
                c <- c + 1
            z <- Array.append z [|row|]
            r <- r + 1
        __ret <- z
        raise Return
        __ret
    with
        | Return -> __ret
and add (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a + b
        raise Return
        __ret
    with
        | Return -> __ret
and sub (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a - b
        raise Return
        __ret
    with
        | Return -> __ret
and mul (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a * b
        raise Return
        __ret
    with
        | Return -> __ret
and div (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- a / b
        raise Return
        __ret
    with
        | Return -> __ret
and exp (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- powf a b
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let m1: float array array = [|[|3.0; 1.0; 4.0|]; [|1.0; 5.0; 9.0|]|]
        let m2: float array array = [|[|2.0; 7.0; 1.0|]; [|8.0; 2.0; 8.0|]|]
        printMatrix "m1:" m1
        printMatrix "m2:" m2
        printfn "%s" ""
        printMatrix "m1 + m2:" (elementWiseMM m1 m2 (unbox<float -> float -> float> add))
        printMatrix "m1 - m2:" (elementWiseMM m1 m2 (unbox<float -> float -> float> sub))
        printMatrix "m1 * m2:" (elementWiseMM m1 m2 (unbox<float -> float -> float> mul))
        printMatrix "m1 / m2:" (elementWiseMM m1 m2 (unbox<float -> float -> float> div))
        printMatrix "m1 ^ m2:" (elementWiseMM m1 m2 (unbox<float -> float -> float> exp))
        printfn "%s" ""
        let s: float = 0.5
        printfn "%s" ("s: " + (string s))
        printMatrix "m1 + s:" (elementWiseMS m1 s (unbox<float -> float -> float> add))
        printMatrix "m1 - s:" (elementWiseMS m1 s (unbox<float -> float -> float> sub))
        printMatrix "m1 * s:" (elementWiseMS m1 s (unbox<float -> float -> float> mul))
        printMatrix "m1 / s:" (elementWiseMS m1 s (unbox<float -> float -> float> div))
        printMatrix "m1 ^ s:" (elementWiseMS m1 s (unbox<float -> float -> float> exp))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
