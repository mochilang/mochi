// Generated 2025-08-25 22:27 +0700

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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec triangular_number (position: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable position = position
    try
        if position < (int64 0) then
            ignore (failwith ("position must be non-negative"))
        __ret <- _floordiv64 (int64 (position * (position + (int64 1)))) (int64 (int64 2))
        raise Return
        __ret
    with
        | Return -> __ret
and test_triangular_number () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (triangular_number (int64 1)) <> (int64 1) then
            ignore (failwith ("triangular_number(1) failed"))
        if (triangular_number (int64 3)) <> (int64 6) then
            ignore (failwith ("triangular_number(3) failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_triangular_number())
        ignore (printfn "%d" (triangular_number (int64 10)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
