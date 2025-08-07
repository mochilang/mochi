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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec expApprox (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable y: float = x
        let mutable is_neg: bool = false
        if x < 0.0 then
            is_neg <- true
            y <- -x
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable n: int = 1
        while n < 30 do
            term <- (term * y) / (float n)
            sum <- sum + term
            n <- n + 1
        if is_neg then
            __ret <- 1.0 / sum
            raise Return
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec round3 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable scaled: float = x * 1000.0
        if scaled >= 0.0 then
            scaled <- scaled + 0.5
        else
            scaled <- scaled - 0.5
        let scaled_int: int = int scaled
        __ret <- (float scaled_int) / 1000.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec charging_capacitor (source_voltage: float) (resistance: float) (capacitance: float) (time_sec: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable source_voltage = source_voltage
    let mutable resistance = resistance
    let mutable capacitance = capacitance
    let mutable time_sec = time_sec
    try
        if source_voltage <= 0.0 then
            failwith ("Source voltage must be positive.")
        if resistance <= 0.0 then
            failwith ("Resistance must be positive.")
        if capacitance <= 0.0 then
            failwith ("Capacitance must be positive.")
        let exponent: float = (-time_sec) / (resistance * capacitance)
        let voltage: float = source_voltage * (1.0 - (expApprox (exponent)))
        __ret <- round3 (voltage)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (charging_capacitor (0.2) (0.9) (8.4) (0.5))
printfn "%g" (charging_capacitor (2.2) (3.5) (2.4) (9.0))
printfn "%g" (charging_capacitor (15.0) (200.0) (20.0) (2.0))
printfn "%g" (charging_capacitor (20.0) (2000.0) (0.0003) (4.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
