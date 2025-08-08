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
type NRResult = {
    mutable _root: float
    mutable _error: float
    mutable _steps: float array
}
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
let rec fail (msg: string) =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    let mutable msg = msg
    try
        printfn "%s" ("error: " + msg)
        __ret
    with
        | Return -> __ret
let rec calc_derivative (f: float -> float) (x: float) (delta_x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable f = f
    let mutable x = x
    let mutable delta_x = delta_x
    try
        __ret <- (float ((float (f (x + (delta_x / 2.0)))) - (float (f (x - (delta_x / 2.0)))))) / delta_x
        raise Return
        __ret
    with
        | Return -> __ret
let rec newton_raphson (f: float -> float) (x0: float) (max_iter: int) (step: float) (max_error: float) (log_steps: bool) =
    let mutable __ret : NRResult = Unchecked.defaultof<NRResult>
    let mutable f = f
    let mutable x0 = x0
    let mutable max_iter = max_iter
    let mutable step = step
    let mutable max_error = max_error
    let mutable log_steps = log_steps
    try
        let mutable a: float = x0
        let mutable _steps: float array = Array.empty<float>
        let mutable i: int = 0
        while i < max_iter do
            if log_steps then
                _steps <- Array.append _steps [|a|]
            let err: float = abs_float (f (a))
            if err < max_error then
                __ret <- { _root = a; _error = err; _steps = _steps }
                raise Return
            let der: float = calc_derivative (f) (a) (step)
            if der = 0.0 then
                fail ("No converging solution found, zero derivative")
                __ret <- { _root = a; _error = err; _steps = _steps }
                raise Return
            a <- a - (float ((float (f (a))) / der))
            i <- i + 1
        fail ("No converging solution found, iteration limit reached")
        __ret <- { _root = a; _error = abs_float (f (a)); _steps = _steps }
        raise Return
        __ret
    with
        | Return -> __ret
let rec poly (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- ((x * x) - (5.0 * x)) + 2.0
        raise Return
        __ret
    with
        | Return -> __ret
let result: NRResult = newton_raphson (unbox<float -> float> poly) (0.4) (20) (0.000001) (0.000001) (false)
printfn "%s" ((("root = " + (_str (result._root))) + ", error = ") + (_str (result._error)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
