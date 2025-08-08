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
let rec fx (x: float) (a: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable a = a
    try
        __ret <- (x * x) - a
        raise Return
        __ret
    with
        | Return -> __ret
let rec fx_derivative (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 2.0 * x
        raise Return
        __ret
    with
        | Return -> __ret
let rec get_initial_point (a: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    try
        let mutable start: float = 2.0
        while start <= a do
            start <- start * start
        __ret <- start
        raise Return
        __ret
    with
        | Return -> __ret
let rec abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec square_root_iterative (a: float) (max_iter: int) (tolerance: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable max_iter = max_iter
    let mutable tolerance = tolerance
    try
        if a < 0.0 then
            failwith ("math domain error")
        let mutable value: float = get_initial_point (a)
        let mutable i: int = 0
        while i < max_iter do
            let prev_value: float = value
            value <- value - ((fx (value) (a)) / (fx_derivative (value)))
            if (abs_float (prev_value - value)) < tolerance then
                __ret <- value
                raise Return
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
let r1: float = square_root_iterative (4.0) (9999) (0.00000000000001)
printfn "%s" (_str (r1))
let r2: float = square_root_iterative (3.2) (9999) (0.00000000000001)
printfn "%s" (_str (r2))
let r3: float = square_root_iterative (140.0) (9999) (0.00000000000001)
printfn "%s" (_str (r3))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
