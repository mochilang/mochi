// Generated 2025-08-12 08:17 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
and pow (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < n do
            result <- result * x
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and factorial (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 2
        while i <= n do
            result <- result * (float i)
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and maclaurin_sin (theta: float) (accuracy: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable theta = theta
    let mutable accuracy = accuracy
    try
        let mutable t: float = theta
        let div: float = floor (t / (2.0 * PI))
        t <- t - ((2.0 * div) * PI)
        let mutable sum: float = 0.0
        let mutable r: int = 0
        while r < accuracy do
            let power: int = (2 * r) + 1
            let sign: float = if (((r % 2 + 2) % 2)) = 0 then 1.0 else (-1.0)
            sum <- sum + ((sign * (pow (t) (power))) / (factorial (power)))
            r <- r + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and maclaurin_cos (theta: float) (accuracy: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable theta = theta
    let mutable accuracy = accuracy
    try
        let mutable t: float = theta
        let div: float = floor (t / (2.0 * PI))
        t <- t - ((2.0 * div) * PI)
        let mutable sum: float = 0.0
        let mutable r: int = 0
        while r < accuracy do
            let power: int = 2 * r
            let sign: float = if (((r % 2 + 2) % 2)) = 0 then 1.0 else (-1.0)
            sum <- sum + ((sign * (pow (t) (power))) / (factorial (power)))
            r <- r + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (maclaurin_sin (10.0) (30)))
printfn "%s" (_str (maclaurin_sin (-10.0) (30)))
printfn "%s" (_str (maclaurin_sin (10.0) (15)))
printfn "%s" (_str (maclaurin_sin (-10.0) (15)))
printfn "%s" (_str (maclaurin_cos (5.0) (30)))
printfn "%s" (_str (maclaurin_cos (-5.0) (30)))
printfn "%s" (_str (maclaurin_cos (10.0) (15)))
printfn "%s" (_str (maclaurin_cos (-10.0) (15)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
