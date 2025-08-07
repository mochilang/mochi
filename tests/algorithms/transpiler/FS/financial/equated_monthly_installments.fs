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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec pow_float (``base``: float) (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec equated_monthly_installments (principal: float) (rate_per_annum: float) (years_to_repay: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable principal = principal
    let mutable rate_per_annum = rate_per_annum
    let mutable years_to_repay = years_to_repay
    try
        if principal <= 0.0 then
            failwith ("Principal borrowed must be > 0")
        if rate_per_annum < 0.0 then
            failwith ("Rate of interest must be >= 0")
        if years_to_repay <= 0 then
            failwith ("Years to repay must be an integer > 0")
        let rate_per_month: float = rate_per_annum / 12.0
        let number_of_payments: int = years_to_repay * 12
        let factor: float = pow_float (1.0 + rate_per_month) (number_of_payments)
        __ret <- ((principal * rate_per_month) * factor) / (factor - 1.0)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (equated_monthly_installments (25000.0) (0.12) (3)))
printfn "%s" (_str (equated_monthly_installments (25000.0) (0.12) (10)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
