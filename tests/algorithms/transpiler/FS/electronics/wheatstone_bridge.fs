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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec wheatstone_solver (resistance_1: float) (resistance_2: float) (resistance_3: float) =
    let mutable __ret : float = Unchecked.defaultof<float>
    let mutable resistance_1 = resistance_1
    let mutable resistance_2 = resistance_2
    let mutable resistance_3 = resistance_3
    try
        if ((resistance_1 <= 0.0) || (resistance_2 <= 0.0)) || (resistance_3 <= 0.0) then
            failwith ("All resistance values must be positive")
        __ret <- (resistance_2 / resistance_1) * resistance_3
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%g" (wheatstone_solver (2.0) (4.0) (5.0))
printfn "%g" (wheatstone_solver (356.0) (234.0) (976.0))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
