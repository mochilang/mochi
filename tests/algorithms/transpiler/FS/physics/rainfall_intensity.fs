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
let rec exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = x
        let mutable is_neg: bool = false
        if x < 0.0 then
            is_neg <- true
            y <- -x
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 30 do
            term <- (term * y) / (float n)
            sum <- sum + term
            n <- n + 1
        if is_neg then
            __ret <- 1.0 / sum
            raise Return
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
and ln_series (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let t: float = (x - 1.0) / (x + 1.0)
        let mutable term: float = t
        let mutable sum: float = 0.0
        let mutable n: int = 1
        while n <= 19 do
            sum <- sum + (term / (float n))
            term <- (term * t) * t
            n <- n + 2
        __ret <- 2.0 * sum
        raise Return
        __ret
    with
        | Return -> __ret
and ln (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = x
        let mutable k: int = 0
        while y >= 10.0 do
            y <- y / 10.0
            k <- k + 1
        while y < 1.0 do
            y <- y * 10.0
            k <- k - 1
        __ret <- (ln_series (y)) + ((float k) * (ln_series (10.0)))
        raise Return
        __ret
    with
        | Return -> __ret
and powf (``base``: float) (exponent: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        __ret <- exp_approx (exponent * (ln (``base``)))
        raise Return
        __ret
    with
        | Return -> __ret
and rainfall_intensity (coefficient_k: float) (coefficient_a: float) (coefficient_b: float) (coefficient_c: float) (return_period: float) (duration: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable coefficient_k = coefficient_k
    let mutable coefficient_a = coefficient_a
    let mutable coefficient_b = coefficient_b
    let mutable coefficient_c = coefficient_c
    let mutable return_period = return_period
    let mutable duration = duration
    try
        if coefficient_k <= 0.0 then
            failwith ("All parameters must be positive.")
        if coefficient_a <= 0.0 then
            failwith ("All parameters must be positive.")
        if coefficient_b <= 0.0 then
            failwith ("All parameters must be positive.")
        if coefficient_c <= 0.0 then
            failwith ("All parameters must be positive.")
        if return_period <= 0.0 then
            failwith ("All parameters must be positive.")
        if duration <= 0.0 then
            failwith ("All parameters must be positive.")
        let numerator: float = coefficient_k * (powf (return_period) (coefficient_a))
        let denominator: float = powf (duration + coefficient_b) (coefficient_c)
        __ret <- numerator / denominator
        raise Return
        __ret
    with
        | Return -> __ret
let r1: float = rainfall_intensity (1000.0) (0.2) (11.6) (0.81) (10.0) (60.0)
printfn "%s" (_str (r1))
let r2: float = rainfall_intensity (1000.0) (0.2) (11.6) (0.81) (10.0) (30.0)
printfn "%s" (_str (r2))
let r3: float = rainfall_intensity (1000.0) (0.2) (11.6) (0.81) (5.0) (60.0)
printfn "%s" (_str (r3))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
