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
let json (arr:obj) =
    match arr with
    | :? (int array array) as a2 ->
        printf "[\n"
        for i in 0 .. a2.Length - 1 do
            let line = String.concat ", " (Array.map string a2.[i] |> Array.toList)
            if i < a2.Length - 1 then
                printfn "  [%s]," line
            else
                printfn "  [%s]" line
        printfn "]"
    | :? (int array) as a1 ->
        let line = String.concat ", " (Array.map string a1 |> Array.toList)
        printfn "[%s]" line
    | _ -> ()
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
open System.Collections.Generic

let rec ohms_law (voltage: float) (current: float) (resistance: float) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable voltage = voltage
    let mutable current = current
    let mutable resistance = resistance
    try
        let mutable zeros: int = 0
        if voltage = 0.0 then
            zeros <- zeros + 1
        if current = 0.0 then
            zeros <- zeros + 1
        if resistance = 0.0 then
            zeros <- zeros + 1
        if zeros <> 1 then
            printfn "%s" ("One and only one argument must be 0")
            __ret <- unbox<System.Collections.Generic.IDictionary<string, float>> (_dictCreate [])
            raise Return
        if resistance < 0.0 then
            printfn "%s" ("Resistance cannot be negative")
            __ret <- unbox<System.Collections.Generic.IDictionary<string, float>> (_dictCreate [])
            raise Return
        if voltage = 0.0 then
            __ret <- _dictCreate [("voltage", current * resistance)]
            raise Return
        if current = 0.0 then
            __ret <- _dictCreate [("current", voltage / resistance)]
            raise Return
        __ret <- _dictCreate [("resistance", voltage / current)]
        raise Return
        __ret
    with
        | Return -> __ret
json (ohms_law (10.0) (0.0) (5.0))
json (ohms_law (-10.0) (1.0) (0.0))
json (ohms_law (0.0) (-1.5) (2.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
