// Generated 2025-08-11 15:32 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    let s = if s.EndsWith(".0") then s.Substring(0, s.Length - 2) else s
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
type Rate = {
    mutable _code: string
    mutable _rate: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rates: Rate array = unbox<Rate array> [|{ _code = "USD"; _rate = 1.0 }; { _code = "EUR"; _rate = 0.9 }; { _code = "INR"; _rate = 83.0 }; { _code = "JPY"; _rate = 156.0 }; { _code = "GBP"; _rate = 0.78 }|]
let rec rate_of (_code: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable _code = _code
    try
        for r in rates do
            if (r._code) = _code then
                __ret <- r._rate
                raise Return
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec convert_currency (from_: string) (``to``: string) (amount: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable from_ = from_
    let mutable ``to`` = ``to``
    let mutable amount = amount
    try
        let from_rate: float = rate_of (from_)
        let to_rate: float = rate_of (``to``)
        if (from_rate = 0.0) || (to_rate = 0.0) then
            __ret <- 0.0
            raise Return
        let usd: float = amount / from_rate
        __ret <- usd * to_rate
        raise Return
        __ret
    with
        | Return -> __ret
let result: float = convert_currency ("USD") ("INR") (10.0)
printfn "%s" (_str (result))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
