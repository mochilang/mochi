// Generated 2025-08-08 17:07 +0700

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
let _dictAdd<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) (v:'V) =
    d.[k] <- v
    d
let _dictCreate<'K,'V when 'K : equality> (pairs:('K * 'V) list) : System.Collections.Generic.IDictionary<'K,'V> =
    let d = System.Collections.Generic.Dictionary<'K, 'V>()
    for (k, v) in pairs do
        d.[k] <- v
    upcast d
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("factorial is undefined for negative numbers")
        let mutable result: int = 1
        let mutable i: int = 2
        while i <= n do
            result <- result * i
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow_float (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec binomial_distribution (successes: int) (trials: int) (prob: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable successes = successes
    let mutable trials = trials
    let mutable prob = prob
    try
        if successes > trials then
            failwith ("successes must be lower or equal to trials")
        if (trials < 0) || (successes < 0) then
            failwith ("the function is defined for non-negative integers")
        if not ((0.0 < prob) && (prob < 1.0)) then
            failwith ("prob has to be in range of 1 - 0")
        let probability: float = (pow_float (prob) (successes)) * (pow_float (1.0 - prob) (trials - successes))
        let numerator: float = float (factorial (trials))
        let denominator: float = float ((factorial (successes)) * (factorial (trials - successes)))
        let coefficient: float = numerator / denominator
        __ret <- probability * coefficient
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
