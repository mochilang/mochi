// Generated 2025-08-09 23:14 +0700

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
let rec fabs (x: float) =
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
let rec reynolds_number (density: float) (velocity: float) (diameter: float) (viscosity: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable density = density
    let mutable velocity = velocity
    let mutable diameter = diameter
    let mutable viscosity = viscosity
    try
        if ((density <= 0.0) || (diameter <= 0.0)) || (viscosity <= 0.0) then
            failwith ("please ensure that density, diameter and viscosity are positive")
        __ret <- ((density * (fabs (velocity))) * diameter) / viscosity
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (reynolds_number (900.0) (2.5) (0.05) (0.4))
printfn "%g" (reynolds_number (450.0) (3.86) (0.078) (0.23))
printfn "%g" (reynolds_number (234.0) (-4.5) (0.3) (0.44))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
