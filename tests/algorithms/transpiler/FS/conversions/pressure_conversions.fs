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
type FromTo = {
    from_factor: float
    to_factor: float
}
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let PRESSURE_CONVERSION: System.Collections.Generic.IDictionary<string, FromTo> = _dictCreate [("atm", { from_factor = 1.0; to_factor = 1.0 }); ("pascal", { from_factor = 0.0000098; to_factor = 101325.0 }); ("bar", { from_factor = 0.986923; to_factor = 1.01325 }); ("kilopascal", { from_factor = 0.00986923; to_factor = 101.325 }); ("megapascal", { from_factor = 9.86923; to_factor = 0.101325 }); ("psi", { from_factor = 0.068046; to_factor = 14.6959 }); ("inHg", { from_factor = 0.0334211; to_factor = 29.9213 }); ("torr", { from_factor = 0.00131579; to_factor = 760.0 })]
let rec pressure_conversion (value: float) (from_type: string) (to_type: string) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable value = value
    let mutable from_type = from_type
    let mutable to_type = to_type
    try
        if not (PRESSURE_CONVERSION.ContainsKey(from_type)) then
            let keys: obj array = (Object.keys(PRESSURE_CONVERSION)).join(", ")
            failwith ((("Invalid 'from_type' value: '" + from_type) + "'  Supported values are:\n") + keys)
        if not (PRESSURE_CONVERSION.ContainsKey(to_type)) then
            let keys: obj array = (Object.keys(PRESSURE_CONVERSION)).join(", ")
            failwith ((("Invalid 'to_type' value: '" + to_type) + ".  Supported values are:\n") + keys)
        let from: FromTo = PRESSURE_CONVERSION.[(string (from_type))]
        let ``to``: FromTo = PRESSURE_CONVERSION.[(string (to_type))]
        __ret <- (value * (from.from_factor)) * (``to``.to_factor)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (pressure_conversion (4.0) ("atm") ("pascal"))
printfn "%g" (pressure_conversion (1.0) ("pascal") ("psi"))
printfn "%g" (pressure_conversion (1.0) ("bar") ("atm"))
printfn "%g" (pressure_conversion (3.0) ("kilopascal") ("bar"))
printfn "%g" (pressure_conversion (2.0) ("megapascal") ("psi"))
printfn "%g" (pressure_conversion (4.0) ("psi") ("torr"))
printfn "%g" (pressure_conversion (1.0) ("inHg") ("atm"))
printfn "%g" (pressure_conversion (1.0) ("torr") ("psi"))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
