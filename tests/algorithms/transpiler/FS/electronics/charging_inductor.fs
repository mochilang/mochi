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
        if x < 0.0 then
            __ret <- 1.0 / (expApprox (-x))
            raise Return
        if x > 1.0 then
            let half: float = expApprox (x / 2.0)
            __ret <- half * half
            raise Return
        let mutable sum: float = 1.0
        let mutable term: float = 1.0
        let mutable n: int = 1
        while n < 20 do
            term <- (term * x) / (float n)
            sum <- sum + term
            n <- n + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec floor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable i: int = int x
        if (float i) > x then
            i <- i - 1
        __ret <- float i
        raise Return
        __ret
    with
        | Return -> __ret
let rec pow10 (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        let mutable result: float = 1.0
        let mutable i: int = 0
        while i < n do
            result <- result * 10.0
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec round (x: float) (n: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable n = n
    try
        let m: float = pow10 (n)
        __ret <- (floor ((x * m) + 0.5)) / m
        raise Return
        __ret
    with
        | Return -> __ret
let rec charging_inductor (source_voltage: float) (resistance: float) (inductance: float) (time: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable source_voltage = source_voltage
    let mutable resistance = resistance
    let mutable inductance = inductance
    let mutable time = time
    try
        if source_voltage <= 0.0 then
            failwith ("Source voltage must be positive.")
        if resistance <= 0.0 then
            failwith ("Resistance must be positive.")
        if inductance <= 0.0 then
            failwith ("Inductance must be positive.")
        let exponent: float = ((-time) * resistance) / inductance
        let current: float = (source_voltage / resistance) * (1.0 - (expApprox (exponent)))
        __ret <- round (current) (3)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (charging_inductor (5.8) (1.5) (2.3) (2.0))
printfn "%g" (charging_inductor (8.0) (5.0) (3.0) (2.0))
printfn "%g" (charging_inductor (8.0) (5.0 * (pow10 (2))) (3.0) (2.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
