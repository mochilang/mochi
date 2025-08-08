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
let _arrset (arr:'a array) (i:int) (v:'a) : 'a array =
    let mutable a = arr
    if i >= a.Length then
        let na = Array.zeroCreate<'a> (i + 1)
        Array.blit a 0 na 0 a.Length
        a <- na
    a.[i] <- v
    a
let rec ceil_int (x: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable x = x
    try
        let mutable n: int = int x
        if (float (float (n))) < x then
            n <- n + 1
        __ret <- n
        raise Return
        __ret
    with
        | Return -> __ret
and explicit_euler (ode_func: float -> float -> float) (y0: float) (x0: float) (step_size: float) (x_end: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable ode_func = ode_func
    let mutable y0 = y0
    let mutable x0 = x0
    let mutable step_size = step_size
    let mutable x_end = x_end
    try
        let mutable n: int = ceil_int ((x_end - x0) / step_size)
        let mutable y: float array = Array.empty<float>
        let mutable i: int = 0
        while i <= n do
            y <- Array.append y [|0.0|]
            i <- i + 1
        y.[0] <- y0
        let mutable x: float = x0
        let mutable k: int = 0
        while k < n do
            y.[k + 1] <- (_idx y (k)) + (float (step_size * (float (ode_func (x) (_idx y (k))))))
            x <- x + step_size
            k <- k + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
and abs_float (a: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable a = a
    try
        __ret <- if a < 0.0 then (-a) else a
        raise Return
        __ret
    with
        | Return -> __ret
and test_explicit_euler () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let f: float -> float -> float =         fun (x: float) (y: float) -> y
        let ys: float array = explicit_euler (f) (1.0) (0.0) (0.01) (5.0)
        let last: float = _idx ys ((Seq.length (ys)) - 1)
        if (abs_float (last - 144.77277243257308)) > 0.001 then
            failwith ("explicit_euler failed")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_explicit_euler()
        let f: float -> float -> float =         fun (x: float) (y: float) -> y
        let ys: float array = explicit_euler (f) (1.0) (0.0) (0.01) (5.0)
        printfn "%g" (_idx ys ((Seq.length (ys)) - 1))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
