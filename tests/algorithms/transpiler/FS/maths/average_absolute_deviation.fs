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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec average_absolute_deviation (nums: int array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable nums = nums
    try
        if (Seq.length (nums)) = 0 then
            failwith ("List is empty")
        let mutable sum: int = 0
        for x in nums do
            sum <- sum + x
        let n: float = float (Seq.length (nums))
        let mean: float = (float sum) / n
        let mutable dev_sum: float = 0.0
        for x in nums do
            dev_sum <- dev_sum + (abs_float ((float x) - mean))
        __ret <- dev_sum / n
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (average_absolute_deviation (unbox<int array> [|0|])))
printfn "%s" (_str (average_absolute_deviation (unbox<int array> [|4; 1; 3; 2|])))
printfn "%s" (_str (average_absolute_deviation (unbox<int array> [|2; 70; 6; 50; 20; 8; 4; 0|])))
printfn "%s" (_str (average_absolute_deviation (unbox<int array> [|-20; 0; 30; 15|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
