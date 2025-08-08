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
let rec _str v =
    let s = sprintf "%A" v
    s.Replace("[|", "[")
     .Replace("|]", "]")
     .Replace("; ", " ")
     .Replace(";", "")
     .Replace("\"", "")
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec sqrt (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable guess: float = if x > 1.0 then (x / 2.0) else 1.0
        let mutable i: int = 0
        while i < 20 do
            guess <- 0.5 * (guess + (x / guess))
            i <- i + 1
        __ret <- guess
        raise Return
        __ret
    with
        | Return -> __ret
let rec runge_kutta_gills (func: float -> float -> float) (x_initial: float) (y_initial: float) (step_size: float) (x_final: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable func = func
    let mutable x_initial = x_initial
    let mutable y_initial = y_initial
    let mutable step_size = step_size
    let mutable x_final = x_final
    try
        if x_initial >= x_final then
            failwith ("The final value of x must be greater than initial value of x.")
        if step_size <= 0.0 then
            failwith ("Step size must be positive.")
        let n: int = int ((x_final - x_initial) / step_size)
        let mutable y: float array = Array.empty<float>
        let mutable i: int = 0
        while i <= n do
            y <- Array.append y [|0.0|]
            i <- i + 1
        y.[0] <- y_initial
        let mutable xi: float = x_initial
        let mutable idx: int = 0
        let root2: float = sqrt (2.0)
        while idx < n do
            let k1: float = step_size * (float (func (xi) (_idx y (idx))))
            let k2: float = step_size * (float (func (xi + (step_size / 2.0)) ((_idx y (idx)) + (k1 / 2.0))))
            let k3: float = step_size * (float (func (xi + (step_size / 2.0)) (((_idx y (idx)) + (((-0.5) + (1.0 / root2)) * k1)) + ((1.0 - (1.0 / root2)) * k2))))
            let k4: float = step_size * (float (func (xi + step_size) (((_idx y (idx)) - ((1.0 / root2) * k2)) + ((1.0 + (1.0 / root2)) * k3))))
            y.[idx + 1] <- (_idx y (idx)) + ((((k1 + ((2.0 - root2) * k2)) + ((2.0 + root2) * k3)) + k4) / 6.0)
            xi <- xi + step_size
            idx <- idx + 1
        __ret <- y
        raise Return
        __ret
    with
        | Return -> __ret
let rec f1 (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- (x - y) / 2.0
        raise Return
        __ret
    with
        | Return -> __ret
let y1: float array = runge_kutta_gills (unbox<float -> float -> float> f1) (0.0) (3.0) (0.2) (5.0)
printfn "%s" (_str (_idx y1 ((Seq.length (y1)) - 1)))
let rec f2 (x: float) (y: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    let mutable y = y
    try
        __ret <- x
        raise Return
        __ret
    with
        | Return -> __ret
let y2: float array = runge_kutta_gills (unbox<float -> float -> float> f2) (-1.0) (0.0) (0.2) (0.0)
printfn "%s" (_str (y2))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
