// Generated 2025-08-07 15:46 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let PI: float = 3.141592653589793
let rec ind_reactance (inductance: float) (frequency: float) (reactance: float) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable inductance = inductance
    let mutable frequency = frequency
    let mutable reactance = reactance
    try
        let mutable zero_count: int = 0
        if inductance = 0.0 then
            zero_count <- zero_count + 1
        if frequency = 0.0 then
            zero_count <- zero_count + 1
        if reactance = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("One and only one argument must be 0")
        if inductance < 0.0 then
            failwith ("Inductance cannot be negative")
        if frequency < 0.0 then
            failwith ("Frequency cannot be negative")
        if reactance < 0.0 then
            failwith ("Inductive reactance cannot be negative")
        if inductance = 0.0 then
            __ret <- _dictCreate [("inductance", reactance / ((2.0 * PI) * frequency))]
            raise Return
        if frequency = 0.0 then
            __ret <- _dictCreate [("frequency", reactance / ((2.0 * PI) * inductance))]
            raise Return
        __ret <- _dictCreate [("reactance", ((2.0 * PI) * frequency) * inductance)]
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%A" (ind_reactance (0.0) (10000.0) (50.0))
printfn "%A" (ind_reactance (0.035) (0.0) (50.0))
printfn "%A" (ind_reactance (0.000035) (1000.0) (0.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
