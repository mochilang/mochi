// Generated 2025-08-17 12:28 +0700

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
let rec signum (num: float) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable num = num
    try
        if num < 0.0 then
            __ret <- -1
            raise Return
        if num > 0.0 then
            __ret <- 1
            raise Return
        __ret <- 0
        raise Return
        __ret
    with
        | Return -> __ret
and test_signum () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (signum (5.0)) <> 1 then
            ignore (failwith ("signum(5) failed"))
        if (signum (-5.0)) <> (-1) then
            ignore (failwith ("signum(-5) failed"))
        if (signum (0.0)) <> 0 then
            ignore (failwith ("signum(0) failed"))
        if (signum (10.5)) <> 1 then
            ignore (failwith ("signum(10.5) failed"))
        if (signum (-10.5)) <> (-1) then
            ignore (failwith ("signum(-10.5) failed"))
        if (signum (0.000001)) <> 1 then
            ignore (failwith ("signum(1e-6) failed"))
        if (signum (-0.000001)) <> (-1) then
            ignore (failwith ("signum(-1e-6) failed"))
        if (signum (123456789.0)) <> 1 then
            ignore (failwith ("signum(123456789) failed"))
        if (signum (-123456789.0)) <> (-1) then
            ignore (failwith ("signum(-123456789) failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_signum())
        ignore (printfn "%d" (signum (12.0)))
        ignore (printfn "%d" (signum (-12.0)))
        ignore (printfn "%d" (signum (0.0)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
