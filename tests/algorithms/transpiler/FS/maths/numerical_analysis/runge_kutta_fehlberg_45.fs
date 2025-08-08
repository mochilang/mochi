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
let rec runge_kutta_fehlberg_45 (func: float -> float -> float) (x_initial: float) (y_initial: float) (step_size: float) (x_final: float) =
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
        let mutable ys: float array = Array.empty<float>
        let mutable x: float = x_initial
        let mutable y: float = y_initial
        ys <- Array.append ys [|y|]
        let mutable i: int = 0
        while i < n do
            let k1: float = step_size * (float (func (x) (y)))
            let k2: float = step_size * (float (func (x + (step_size / 4.0)) (y + (k1 / 4.0))))
            let k3: float = step_size * (float (func (x + ((3.0 / 8.0) * step_size)) ((y + ((3.0 / 32.0) * k1)) + ((9.0 / 32.0) * k2))))
            let k4: float = step_size * (float (func (x + ((12.0 / 13.0) * step_size)) (((y + ((1932.0 / 2197.0) * k1)) - ((7200.0 / 2197.0) * k2)) + ((7296.0 / 2197.0) * k3))))
            let k5: float = step_size * (float (func (x + step_size) ((((y + ((439.0 / 216.0) * k1)) - (8.0 * k2)) + ((3680.0 / 513.0) * k3)) - ((845.0 / 4104.0) * k4))))
            let k6: float = step_size * (float (func (x + (step_size / 2.0)) (((((y - ((8.0 / 27.0) * k1)) + (2.0 * k2)) - ((3544.0 / 2565.0) * k3)) + ((1859.0 / 4104.0) * k4)) - ((11.0 / 40.0) * k5))))
            y <- ((((y + ((16.0 / 135.0) * k1)) + ((6656.0 / 12825.0) * k3)) + ((28561.0 / 56430.0) * k4)) - ((9.0 / 50.0) * k5)) + ((2.0 / 55.0) * k6)
            x <- x + step_size
            ys <- Array.append ys [|y|]
            i <- i + 1
        __ret <- ys
        raise Return
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        let rec f1 (x: float) (y: float) =
            let mutable __ret : float = Unchecked.defaultof<float>
            let mutable x = x
            let mutable y = y
            try
                __ret <- 1.0 + (y * y)
                raise Return
                __ret
            with
                | Return -> __ret
        let y1: float array = runge_kutta_fehlberg_45 (unbox<float -> float -> float> f1) (0.0) (0.0) (0.2) (1.0)
        printfn "%g" (_idx y1 (1))
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
        let y2: float array = runge_kutta_fehlberg_45 (unbox<float -> float -> float> f2) (-1.0) (0.0) (0.2) (0.0)
        printfn "%g" (_idx y2 (1))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
