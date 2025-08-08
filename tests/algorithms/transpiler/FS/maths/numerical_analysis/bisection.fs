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
let rec abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- -x
            raise Return
        else
            __ret <- x
            raise Return
        __ret
    with
        | Return -> __ret
and bisection (f: float -> float) (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable a = a
    let mutable b = b
    try
        let mutable start: float = a
        let mutable ``end``: float = b
        if (f (a)) = 0.0 then
            __ret <- a
            raise Return
        if (f (b)) = 0.0 then
            __ret <- b
            raise Return
        if ((f (a)) * (f (b))) > 0.0 then
            failwith ("could not find root in given interval.")
        let mutable mid: float = start + ((``end`` - start) / 2.0)
        while (abs_float (start - mid)) > 0.0000001 do
            let fmid: float = f (mid)
            if fmid = 0.0 then
                __ret <- mid
                raise Return
            if (fmid * (f (start))) < 0.0 then
                ``end`` <- mid
            else
                start <- mid
            mid <- start + ((``end`` - start) / 2.0)
        __ret <- mid
        raise Return
        __ret
    with
        | Return -> __ret
and f (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (((x * x) * x) - (2.0 * x)) - 5.0
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        printfn "%s" (_str (bisection (unbox<float -> float> f) (1.0) (1000.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
