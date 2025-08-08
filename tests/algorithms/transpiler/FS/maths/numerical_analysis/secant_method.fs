// Generated 2025-08-08 18:09 +0700

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
let rec _str v =
    let s = sprintf "%A" v
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
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable i: int = 1
        while i <= 20 do
            term <- (term * x) / (float i)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec f (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (8.0 * x) - (2.0 * (exp_approx (-x)))
        raise Return
        __ret
    with
        | Return -> __ret
let rec secant_method (lower_bound: float) (upper_bound: float) (repeats: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable lower_bound = lower_bound
    let mutable upper_bound = upper_bound
    let mutable repeats = repeats
    try
        let mutable x0: float = lower_bound
        let mutable x1: float = upper_bound
        let mutable i: int = 0
        while i < repeats do
            let fx1: float = f (x1)
            let fx0: float = f (x0)
            let new_x: float = x1 - ((fx1 * (x1 - x0)) / (fx1 - fx0))
            x0 <- x1
            x1 <- new_x
            i <- i + 1
        __ret <- x1
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (secant_method (1.0) (3.0) (2)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
