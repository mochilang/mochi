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
let rec straight_line_depreciation (useful_years: int) (purchase_value: float) (residual_value: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable useful_years = useful_years
    let mutable purchase_value = purchase_value
    let mutable residual_value = residual_value
    try
        if useful_years < 1 then
            failwith ("Useful years cannot be less than 1")
        if purchase_value < 0.0 then
            failwith ("Purchase value cannot be less than zero")
        if purchase_value < residual_value then
            failwith ("Purchase value cannot be less than residual value")
        let depreciable_cost: float = purchase_value - residual_value
        let annual_expense: float = depreciable_cost / (1.0 * (float useful_years))
        let mutable expenses: float array = [||]
        let mutable accumulated: float = 0.0
        let mutable period: int = 0
        while period < useful_years do
            if period <> (useful_years - 1) then
                accumulated <- accumulated + annual_expense
                expenses <- Array.append expenses [|annual_expense|]
            else
                let end_year_expense: float = depreciable_cost - accumulated
                expenses <- Array.append expenses [|end_year_expense|]
            period <- period + 1
        __ret <- expenses
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (straight_line_depreciation (10) (1100.0) (100.0)))
printfn "%s" (_str (straight_line_depreciation (6) (1250.0) (50.0)))
printfn "%s" (_str (straight_line_depreciation (4) (1001.0) (0.0)))
printfn "%s" (_str (straight_line_depreciation (11) (380.0) (50.0)))
printfn "%s" (_str (straight_line_depreciation (1) (4985.0) (100.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
