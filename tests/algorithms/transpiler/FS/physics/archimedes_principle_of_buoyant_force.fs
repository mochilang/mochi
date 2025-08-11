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
let G: float = 9.80665
let rec archimedes_principle (fluid_density: float) (volume: float) (gravity: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fluid_density = fluid_density
    let mutable volume = volume
    let mutable gravity = gravity
    try
        if fluid_density <= 0.0 then
            failwith ("Impossible fluid density")
        if volume <= 0.0 then
            failwith ("Impossible object volume")
        if gravity < 0.0 then
            failwith ("Impossible gravity")
        __ret <- (fluid_density * volume) * gravity
        raise Return
        __ret
    with
        | Return -> __ret
let rec archimedes_principle_default (fluid_density: float) (volume: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable fluid_density = fluid_density
    let mutable volume = volume
    try
        let res: float = archimedes_principle (fluid_density) (volume) (G)
        __ret <- res
        raise Return
        __ret
    with
        | Return -> __ret
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
