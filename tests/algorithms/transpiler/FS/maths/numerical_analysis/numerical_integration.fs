// Generated 2025-08-08 18:09 +0700

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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec abs_float (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        if x < 0.0 then
            __ret <- -x
            raise Return
        else
            __ret <- x
            raise Return
        __ret
    with
        | Return -> __ret
let rec trapezoidal_area (f: float -> float) (x_start: float) (x_end: float) (steps: int) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable x_start = x_start
    let mutable x_end = x_end
    let mutable steps = steps
    try
        let step: float = (x_end - x_start) / (float steps)
        let mutable x1: float = x_start
        let mutable fx1: float = f (x_start)
        let mutable area: float = 0.0
        let mutable i: int = 0
        while i < steps do
            let x2: float = x1 + step
            let fx2: float = f (x2)
            area <- area + (((abs_float (fx2 + fx1)) * step) / 2.0)
            x1 <- x2
            fx1 <- fx2
            i <- i + 1
        __ret <- area
        raise Return
        __ret
    with
        | Return -> __ret
let rec f (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- (x * x) * x
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%s" ("f(x) = x^3")
printfn "%s" ("The area between the curve, x = -10, x = 10 and the x axis is:")
let mutable i: int = 10
while i <= 100000 do
    let mutable area: float = trapezoidal_area (unbox<float -> float> f) (-5.0) (5.0) (i)
    printfn "%s" ((("with " + (_str (i))) + " steps: ") + (_str (area)))
    i <- int ((int64 i) * (int64 10))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
