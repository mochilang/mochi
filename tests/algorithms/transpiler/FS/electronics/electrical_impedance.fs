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

let rec sqrtApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x <= 0.0 then
            __ret <- 0.0
            raise Return
        let mutable guess: float = x / 2.0
        let mutable i: int = 0
        while i < 20 do
            guess <- (guess + (x / guess)) / 2.0
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec electrical_impedance (resistance: float) (reactance: float) (impedance: float) =
    let mutable __ret : System.Collections.Generic.IDictionary<string, float> = Unchecked.defaultof<System.Collections.Generic.IDictionary<string, float>>
    let mutable resistance = resistance
    let mutable reactance = reactance
    let mutable impedance = impedance
    try
        let mutable zero_count: int = 0
        if resistance = 0.0 then
            zero_count <- zero_count + 1
        if reactance = 0.0 then
            zero_count <- zero_count + 1
        if impedance = 0.0 then
            zero_count <- zero_count + 1
        if zero_count <> 1 then
            failwith ("One and only one argument must be 0")
        if resistance = 0.0 then
            let value: float = sqrtApprox ((impedance * impedance) - (reactance * reactance))
            __ret <- _dictCreate [("resistance", value)]
            raise Return
        else
            if reactance = 0.0 then
                let value: float = sqrtApprox ((impedance * impedance) - (resistance * resistance))
                __ret <- _dictCreate [("reactance", value)]
                raise Return
            else
                if impedance = 0.0 then
                    let value: float = sqrtApprox ((resistance * resistance) + (reactance * reactance))
                    __ret <- _dictCreate [("impedance", value)]
                    raise Return
                else
                    failwith ("Exactly one argument must be 0")
        __ret
    with
        | Return -> __ret
printfn "%A" (electrical_impedance (3.0) (4.0) (0.0))
printfn "%A" (electrical_impedance (0.0) (4.0) (5.0))
printfn "%A" (electrical_impedance (3.0) (0.0) (5.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
