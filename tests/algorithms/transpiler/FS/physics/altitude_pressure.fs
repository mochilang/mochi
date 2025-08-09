// Generated 2025-08-09 16:21 +0700

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
let rec to_float (x: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (float x) * 1.0
        raise Return
        __ret
    with
        | Return -> __ret
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            failwith ("ln domain error")
        let y: float = (x - 1.0) / (x + 1.0)
        let y2: float = y * y
        let mutable term: float = y
        let mutable sum: float = 0.0
        let mutable k: int = 0
        while k < 10 do
            let denom: float = to_float (int (((int64 2) * (int64 k)) + (int64 1)))
            sum <- sum + (term / denom)
            term <- term * y2
            k <- k + 1
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and exp (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 20 do
            term <- (term * x) / (to_float (n))
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and pow_float (``base``: float) (exponent: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        __ret <- exp (exponent * (ln (``base``)))
        raise Return
        __ret
    with
        | Return -> __ret
and get_altitude_at_pressure (pressure: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable pressure = pressure
    try
        if pressure > 101325.0 then
            failwith ("Value Higher than Pressure at Sea Level !")
        if pressure < 0.0 then
            failwith ("Atmospheric Pressure can not be negative !")
        let ratio: float = pressure / 101325.0
        __ret <- 44330.0 * (1.0 - (pow_float (ratio) (1.0 / 5.5255)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (get_altitude_at_pressure (100000.0)))
printfn "%s" (_str (get_altitude_at_pressure (101325.0)))
printfn "%s" (_str (get_altitude_at_pressure (80000.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
