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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let ENERGY_CONVERSION: System.Collections.Generic.IDictionary<string, float> = _dictCreate [("joule", 1.0); ("kilojoule", 1000.0); ("megajoule", 1000000.0); ("gigajoule", 1000000000.0); ("wattsecond", 1.0); ("watthour", 3600.0); ("kilowatthour", 3600000.0); ("newtonmeter", 1.0); ("calorie_nutr", 4186.8); ("kilocalorie_nutr", 4186800.0); ("electronvolt", 0.0000000000000000001602176634); ("britishthermalunit_it", 1055.05585); ("footpound", 1.355818)]
let rec energy_conversion (from_type: string) (to_type: string) (value: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable from_type = from_type
    let mutable to_type = to_type
    let mutable value = value
    try
        if ((ENERGY_CONVERSION.ContainsKey(from_type)) = false) || ((ENERGY_CONVERSION.ContainsKey(to_type)) = false) then
            failwith ("Incorrect 'from_type' or 'to_type'")
        __ret <- (value * (ENERGY_CONVERSION.[(string (from_type))])) / (ENERGY_CONVERSION.[(string (to_type))])
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" (_str (energy_conversion ("joule") ("kilojoule") (1.0)))
printfn "%s" (_str (energy_conversion ("kilowatthour") ("joule") (10.0)))
printfn "%s" (_str (energy_conversion ("britishthermalunit_it") ("footpound") (1.0)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
