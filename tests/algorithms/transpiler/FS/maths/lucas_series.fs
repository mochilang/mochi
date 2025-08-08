// Generated 2025-08-08 17:35 +0700

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
let rec recursive_lucas_number (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n = 0 then
            __ret <- 2
            raise Return
        if n = 1 then
            __ret <- 1
            raise Return
        __ret <- (recursive_lucas_number (n - 1)) + (recursive_lucas_number (n - 2))
        raise Return
        __ret
    with
        | Return -> __ret
let rec dynamic_lucas_number (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        let mutable a: int = 2
        let mutable b: int = 1
        let mutable i: int = 0
        while i < n do
            let next: int = a + b
            a <- b
            b <- next
            i <- i + 1
        __ret <- a
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (recursive_lucas_number (1)))
printfn "%s" (_str (recursive_lucas_number (20)))
printfn "%s" (_str (recursive_lucas_number (0)))
printfn "%s" (_str (recursive_lucas_number (5)))
printfn "%s" (_str (dynamic_lucas_number (1)))
printfn "%s" (_str (dynamic_lucas_number (20)))
printfn "%s" (_str (dynamic_lucas_number (0)))
printfn "%s" (_str (dynamic_lucas_number (25)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
