// Generated 2025-08-08 17:35 +0700

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
let _dictGet<'K,'V when 'K : equality> (d:System.Collections.Generic.IDictionary<'K,'V>) (k:'K) : 'V =
    match d.TryGetValue(k) with
    | true, v -> v
    | _ -> Unchecked.defaultof<'V>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrt_newton (n: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable n = n
    try
        if n = 0.0 then
            __ret <- 0.0
            raise Return
        let mutable x: float = n
        let mutable i: int = 0
        while i < 20 do
            x <- (x + (n / x)) / 2.0
            i <- i + 1
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec hypot (a: float) (b: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    let mutable b = b
    try
        __ret <- sqrt_newton ((a * a) + (b * b))
        raise Return
        __ret
    with
        | Return -> __ret
let rec line_length (fnc: float -> float) (x_start: float) (x_end: float) (steps: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fnc = fnc
    let mutable x_start = x_start
    let mutable x_end = x_end
    let mutable steps = steps
    try
        let mutable x1: float = x_start
        let mutable fx1: float = fnc (x_start)
        let mutable length: float = 0.0
        let mutable i: int = 0
        let step: float = (x_end - x_start) / (1.0 * (float steps))
        while i < steps do
            let x2: float = step + x1
            let fx2: float = fnc (x2)
            length <- length + (hypot (x2 - x1) (fx2 - fx1))
            x1 <- x2
            fx1 <- fx2
            i <- i + 1
        __ret <- length
        raise Return
        __ret
    with
        | Return -> __ret
let rec f1 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let rec f2 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- 1.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec f3 (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (x * x) / 10.0
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (line_length (unbox<float -> float> f1) (0.0) (1.0) (10))
printfn "%g" (line_length (unbox<float -> float> f2) (-5.5) (4.5) (100))
printfn "%g" (line_length (unbox<float -> float> f3) (0.0) (10.0) (1000))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
