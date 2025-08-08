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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let rec ceil_float (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let i: int = int x
        if x > (float i) then
            __ret <- i + 1
            raise Return
        __ret <- i
        raise Return
        __ret
    with
        | Return -> __ret
and exp_approx (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
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
and euler_modified (ode_func: float -> float -> float) (y0: float) (x0: float) (step: float) (x_end: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable ode_func = ode_func
    let mutable y0 = y0
    let mutable x0 = x0
    let mutable step = step
    let mutable x_end = x_end
    try
        let mutable n: int = ceil_float ((x_end - x0) / step)
        let mutable y: float array = unbox<float array> [|y0|]
        let mutable x: float = x0
        let mutable k: int = 0
        while k < n do
            let y_predict: float = (_idx y (k)) + (float (step * (float (ode_func (x) (_idx y (k))))))
            let slope1: float = ode_func (x) (_idx y (k))
            let slope2: float = ode_func (x + step) (y_predict)
            let y_next: float = (_idx y (k)) + ((step / 2.0) * (slope1 + slope2))
            y <- Array.append y [|y_next|]
            x <- x + step
            k <- k + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
and f1 (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- (((-2.0) * x) * y) * y
        raise Return
        __ret
    with
        | Return -> __ret
and f2 (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- ((-2.0) * y) + (((x * x) * x) * (exp_approx ((-2.0) * x)))
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let y1: float array = euler_modified (unbox<float -> float -> float> f1) (1.0) (0.0) (0.2) (1.0)
        printfn "%g" (_idx y1 ((Seq.length (y1)) - 1))
        let y2: float array = euler_modified (unbox<float -> float -> float> f2) (1.0) (0.0) (0.1) (0.3)
        printfn "%g" (_idx y2 ((Seq.length (y2)) - 1))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
