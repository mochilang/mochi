// Generated 2025-08-07 10:31 +0700

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
let _idx (arr:'a array) (i:int) : 'a =
    if i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec pow10 (exp: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable exp = exp
    try
        let mutable result: float = 1.0
        if exp >= 0 then
            let mutable i: int = 0
            while i < exp do
                result <- result * 10.0
                i <- i + 1
        else
            let mutable i: int = 0
            while i < (0 - exp) do
                result <- result / 10.0
                i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let KILOGRAM_CHART: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("kilogram", 1.0); ("gram", 1000.0); ("milligram", 1000000.0); ("metric-ton", 0.001); ("long-ton", 0.0009842073); ("short-ton", 0.0011023122); ("pound", 2.2046244202); ("stone", 0.1574731728); ("ounce", 35.273990723); ("carrat", 5000.0); ("atomic-mass-unit", 6.022136652 * (pow10 (26)))]
let WEIGHT_TYPE_CHART: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("kilogram", 1.0); ("gram", 0.001); ("milligram", 0.000001); ("metric-ton", 1000.0); ("long-ton", 1016.04608); ("short-ton", 907.184); ("pound", 0.453592); ("stone", 6.35029); ("ounce", 0.0283495); ("carrat", 0.0002); ("atomic-mass-unit", 1.660540199 * (pow10 (-27)))]
let rec weight_conversion (from_type: string) (to_type: string) (value: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable from_type = from_type
    let mutable to_type = to_type
    let mutable value = value
    try
        let has_to: bool = KILOGRAM_CHART.ContainsKey(to_type)
        let has_from: bool = WEIGHT_TYPE_CHART.ContainsKey(from_type)
        if has_to && has_from then
            __ret <- (value * (KILOGRAM_CHART.[(string (to_type))])) * (WEIGHT_TYPE_CHART.[(string (from_type))])
            raise Return
        printfn "%s" ("Invalid 'from_type' or 'to_type'")
        __ret <- 0.0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (weight_conversion ("kilogram") ("gram") (1.0))
printfn "%g" (weight_conversion ("gram") ("pound") (3.0))
printfn "%g" (weight_conversion ("ounce") ("kilogram") (3.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
