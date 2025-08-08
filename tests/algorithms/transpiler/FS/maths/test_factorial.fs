// Generated 2025-08-08 18:58 +0700

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
let rec factorial (n: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable n = n
    try
        if n < 0 then
            failwith ("factorial() not defined for negative values")
        let mutable value: int = 1
        let mutable i: int = 1
        while i <= n do
            value <- int ((int64 value) * (int64 i))
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
        __ret <- int ((int64 n) * (int64 (factorial_recursive (n - 1))))
        raise Return
        __ret
    with
        | Return -> __ret
and test_zero () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (factorial (0)) <> 1 then
            failwith ("factorial(0) failed")
        if (factorial_recursive (0)) <> 1 then
            failwith ("factorial_recursive(0) failed")
        __ret
    with
        | Return -> __ret
and test_positive_integers () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (factorial (1)) <> 1 then
            failwith ("factorial(1) failed")
        if (factorial_recursive (1)) <> 1 then
            failwith ("factorial_recursive(1) failed")
        if (factorial (5)) <> 120 then
            failwith ("factorial(5) failed")
        if (factorial_recursive (5)) <> 120 then
            failwith ("factorial_recursive(5) failed")
        if (factorial (7)) <> 5040 then
            failwith ("factorial(7) failed")
        if (factorial_recursive (7)) <> 5040 then
            failwith ("factorial_recursive(7) failed")
        __ret
    with
        | Return -> __ret
and test_large_number () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        if (factorial (10)) <> 3628800 then
            failwith ("factorial(10) failed")
        if (factorial_recursive (10)) <> 3628800 then
            failwith ("factorial_recursive(10) failed")
        __ret
    with
        | Return -> __ret
and run_tests () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        test_zero()
        test_positive_integers()
        test_large_number()
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : unit = Unchecked.defaultof<unit>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        run_tests()
        printfn "%d" (factorial (6))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
main()
