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
let _idx (arr:'a array) (i:int) : 'a =
    if not (obj.ReferenceEquals(arr, null)) && i >= 0 && i < arr.Length then arr.[i] else Unchecked.defaultof<'a>
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let PI: float = 3.141592653589793
let rec abs (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        __ret <- if x < 0.0 then (-x) else x
        raise Return
        __ret
    with
        | Return -> __ret
let rec to_radians (deg: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable deg = deg
    try
        __ret <- (deg * PI) / 180.0
        raise Return
        __ret
    with
        | Return -> __ret
let rec sin_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = x
        let mutable sum: float = x
        let mutable i: int = 1
        while i < 10 do
            let k1: float = 2.0 * (float i)
            let k2: float = k1 + 1.0
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec cos_taylor (x: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable x = x
    try
        let mutable term: float = 1.0
        let mutable sum: float = 1.0
        let mutable i: int = 1
        while i < 10 do
            let k1: float = (2.0 * (float i)) - 1.0
            let k2: float = 2.0 * (float i)
            term <- (((-term) * x) * x) / (k1 * k2)
            sum <- sum + term
            i <- i + 1
        __ret <- sum
        raise Return
        __ret
    with
        | Return -> __ret
let rec rect (mag: float) (angle: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable mag = mag
    let mutable angle = angle
    try
        let c: float = cos_taylor (angle)
        let s: float = sin_taylor (angle)
        __ret <- unbox<float array> [|mag * c; mag * s|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec multiply (a: float array) (b: float array) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable a = a
    let mutable b = b
    try
        __ret <- unbox<float array> [|((_idx a (0)) * (_idx b (0))) - ((_idx a (1)) * (_idx b (1))); ((_idx a (0)) * (_idx b (1))) + ((_idx a (1)) * (_idx b (0)))|]
        raise Return
        __ret
    with
        | Return -> __ret
let rec apparent_power (voltage: float) (current: float) (voltage_angle: float) (current_angle: float) =
    let mutable __ret : float array = Unchecked.defaultof<float array>
    let mutable voltage = voltage
    let mutable current = current
    let mutable voltage_angle = voltage_angle
    let mutable current_angle = current_angle
    try
        let vrad: float = to_radians (voltage_angle)
        let irad: float = to_radians (current_angle)
        let vrect: float array = rect (voltage) (vrad)
        let irect: float array = rect (current) (irad)
        let result: float array = multiply (vrect) (irect)
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
let rec approx_equal (a: float array) (b: float array) (eps: float) =
    let mutable __ret : bool = Unchecked.defaultof<bool>
    let mutable a = a
    let mutable b = b
    let mutable eps = eps
    try
        __ret <- ((abs ((_idx a (0)) - (_idx b (0)))) < eps) && ((abs ((_idx a (1)) - (_idx b (1)))) < eps)
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
