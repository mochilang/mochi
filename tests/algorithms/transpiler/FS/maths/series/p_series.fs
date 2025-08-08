// Generated 2025-08-08 18:58 +0700

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
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let _repr v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", ", ")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow_string (``base``: int) (exp: int) =
    let mutable __ret : string = Unchecked.defaultof<string>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        if exp >= 0 then
            let mutable res: int = 1
            let mutable i: int = 0
            while i < exp do
                res <- int ((int64 res) * (int64 ``base``))
                i <- i + 1
            __ret <- _str (res)
            raise Return
        let mutable e: int = -exp
        let mutable res: float = 1.0
        let mutable b: float = (float ``base``) * 1.0
        let mutable i: int = 0
        while i < e do
            res <- res * b
            i <- i + 1
        let value: float = 1.0 / res
        __ret <- _str (value)
        raise Return
        __ret
    with
        | Return -> __ret
let rec p_series (nth_term: int) (power: int) =
    let mutable __ret : string array = Unchecked.defaultof<string array>
    let mutable nth_term = nth_term
    let mutable power = power
    try
        let mutable series: string array = Array.empty<string>
        if nth_term <= 0 then
            __ret <- series
            raise Return
        let mutable i: int = 1
        while i <= nth_term do
            if i = 1 then
                series <- Array.append series [|"1"|]
            else
                series <- Array.append series [|("1 / " + (pow_string (i) (power)))|]
            i <- i + 1
        __ret <- series
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_repr (p_series (5) (2)))
printfn "%s" (_repr (p_series (-5) (2)))
printfn "%s" (_repr (p_series (5) (-2)))
printfn "%s" (_repr (p_series (0) (0)))
printfn "%s" (_repr (p_series (1) (1)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
