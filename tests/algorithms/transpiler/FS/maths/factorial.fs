// Generated 2025-08-12 08:17 +0700

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
let rec factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("factorial() not defined for negative values")
        let mutable value: int = 1
        let mutable i: int = 1
        while i <= n do
            value <- value * i
            i <- i + 1
        __ret <- value
        raise Return
        __ret
    with
        | Return -> __ret
and factorial_recursive (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("factorial() not defined for negative values")
        if n <= 1 then
            __ret <- 1
            raise Return
        __ret <- n * (factorial_recursive (n - 1))
        raise Return
        __ret
    with
        | Return -> __ret
and test_factorial () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let mutable i: int = 0
        while i <= 10 do
            if (factorial (i)) <> (factorial_recursive (i)) then
                failwith ("mismatch between factorial and factorial_recursive")
            i <- i + 1
        if (factorial (6)) <> 720 then
            failwith ("factorial(6) should be 720")
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        test_factorial()
        printfn "%d" (factorial (6))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
