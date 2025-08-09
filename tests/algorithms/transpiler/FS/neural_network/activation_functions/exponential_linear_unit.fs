// Generated 2025-08-09 10:14 +0700

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
let rec exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable i: int = 1
        let absx: float = if x < 0.0 then (-x) else x
        while i <= 20 do
            term <- (term * absx) / (float i)
            sum <- sum + term
            i <- i + 1
        if x < 0.0 then
            __ret <- 1.0 / sum
            raise Return
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec exponential_linear_unit (vector: float array) (alpha: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable vector = vector
    let mutable alpha = alpha
    try
        let mutable result: float array = Array.empty<float>
        let mutable i: int = 0
        while i < (Seq.length (vector)) do
            let v: float = _idx vector (int i)
            if v > 0.0 then
                result <- Array.append result [|v|]
            else
                let neg: float = alpha * ((exp_approx (v)) - 1.0)
                result <- Array.append result [|neg|]
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (exponential_linear_unit (unbox<float array> [|2.3; 0.6; -2.0; -3.8|]) (0.3)))
printfn "%s" (_str (exponential_linear_unit (unbox<float array> [|-9.2; -0.3; 0.45; -4.56|]) (0.067)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
