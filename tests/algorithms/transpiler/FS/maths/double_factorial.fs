// Generated 2025-08-16 14:41 +0700

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
let rec double_factorial_recursive (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            ignore (failwith ("double_factorial_recursive() not defined for negative values"))
        if n <= 1 then
            __ret <- 1
            raise Return
        __ret <- n * (double_factorial_recursive (n - 2))
        raise Return
        __ret
    with
        | Return -> __ret
and double_factorial_iterative (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            ignore (failwith ("double_factorial_iterative() not defined for negative values"))
        let mutable result: int = 1
        let mutable i: int = n
        while i > 0 do
            result <- result * i
            i <- i - 2
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and test_double_factorial () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (double_factorial_recursive (0)) <> 1 then
            ignore (failwith ("0!! recursive failed"))
        if (double_factorial_iterative (0)) <> 1 then
            ignore (failwith ("0!! iterative failed"))
        if (double_factorial_recursive (1)) <> 1 then
            ignore (failwith ("1!! recursive failed"))
        if (double_factorial_iterative (1)) <> 1 then
            ignore (failwith ("1!! iterative failed"))
        if (double_factorial_recursive (5)) <> 15 then
            ignore (failwith ("5!! recursive failed"))
        if (double_factorial_iterative (5)) <> 15 then
            ignore (failwith ("5!! iterative failed"))
        if (double_factorial_recursive (6)) <> 48 then
            ignore (failwith ("6!! recursive failed"))
        if (double_factorial_iterative (6)) <> 48 then
            ignore (failwith ("6!! iterative failed"))
        let mutable n: int = 0
        while n <= 10 do
            if (double_factorial_recursive (n)) <> (double_factorial_iterative (n)) then
                ignore (failwith ("double factorial mismatch"))
            n <- n + 1
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_double_factorial())
        ignore (printfn "%d" (double_factorial_iterative (10)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
