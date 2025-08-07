// Generated 2025-08-07 16:27 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec powf (``base``: float) (exponent: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exponent = exponent
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exponent do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec round2 (value: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    try
        if value >= 0.0 then
            let scaled: int = int ((value * 100.0) + 0.5)
            __ret <- (float scaled) / 100.0
            raise Return
        let scaled: int = int ((value * 100.0) - 0.5)
        __ret <- (float scaled) / 100.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec present_value (discount_rate: float) (cash_flows: float array) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable discount_rate = discount_rate
    let mutable cash_flows = cash_flows
    try
        if discount_rate < 0.0 then
            failwith ("Discount rate cannot be negative")
        if (Seq.length (cash_flows)) = 0 then
            failwith ("Cash flows list cannot be empty")
        let mutable pv: float = 0.0
        let mutable i: int = 0
        let factor: float = 1.0 + discount_rate
        while i < (Seq.length (cash_flows)) do
            let cf: float = _idx cash_flows (i)
            pv <- pv + (cf / (powf (factor) (i)))
            i <- i + 1
        __ret <- round2 (pv)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (present_value (0.13) (unbox<float array> [|10.0; 20.7; -293.0; 297.0|])))
printfn "%s" (_str (present_value (0.07) (unbox<float array> [|-109129.39; 30923.23; 15098.93; 29734.0; 39.0|])))
printfn "%s" (_str (present_value (0.07) (unbox<float array> [|109129.39; 30923.23; 15098.93; 29734.0; 39.0|])))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
