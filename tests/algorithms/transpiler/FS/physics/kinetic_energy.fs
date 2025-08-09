// Generated 2025-08-09 16:21 +0700

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
let rec kinetic_energy (mass: float) (velocity: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable mass = mass
    let mutable velocity = velocity
    try
        if mass < 0.0 then
            failwith ("The mass of a body cannot be negative")
        let mutable v: float = velocity
        if v < 0.0 then
            v <- -v
        __ret <- ((0.5 * mass) * v) * v
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (kinetic_energy (10.0) (10.0))
printfn "%g" (kinetic_energy (0.0) (10.0))
printfn "%g" (kinetic_energy (10.0) (0.0))
printfn "%g" (kinetic_energy (20.0) (-20.0))
printfn "%g" (kinetic_energy (0.0) (0.0))
printfn "%g" (kinetic_energy (2.0) (2.0))
printfn "%g" (kinetic_energy (100.0) (100.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
