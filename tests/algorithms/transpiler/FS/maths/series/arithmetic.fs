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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
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
let rec is_arithmetic_series (xs: float array) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            failwith ("Input list must be a non empty list")
        if (Seq.length (xs)) = 1 then
            __ret <- true
            raise Return
        let diff: float = (_idx xs (1)) - (_idx xs (0))
        let mutable i: int = 0
        while i < ((Seq.length (xs)) - 1) do
            if ((_idx xs (i + 1)) - (_idx xs (i))) <> diff then
                __ret <- false
                raise Return
            i <- i + 1
        __ret <- true
        raise Return
        __ret
    with
        | Return -> __ret
let rec arithmetic_mean (xs: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable xs = xs
    try
        if (Seq.length (xs)) = 0 then
            failwith ("Input list must be a non empty list")
        let mutable total: float = 0.0
        let mutable i: int = 0
        while i < (Seq.length (xs)) do
            total <- total + (_idx xs (i))
            i <- i + 1
        __ret <- total / (float (Seq.length (xs)))
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (is_arithmetic_series (unbox<float array> [|2.0; 4.0; 6.0|])))
printfn "%s" (_str (is_arithmetic_series (unbox<float array> [|3.0; 6.0; 12.0; 24.0|])))
printfn "%s" (_str (arithmetic_mean (unbox<float array> [|2.0; 4.0; 6.0|])))
printfn "%s" (_str (arithmetic_mean (unbox<float array> [|3.0; 6.0; 9.0; 12.0|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
