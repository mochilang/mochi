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
let _floordiv64 (a:int64) (b:int64) : int64 =
    let q = a / b
    let r = a % b
    if r <> 0L && ((a < 0L) <> (b < 0L)) then q - 1L else q
let rec sum_of_series (first_term: int) (common_diff: int) (num_of_terms: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable first_term = first_term
    let mutable common_diff = common_diff
    let mutable num_of_terms = num_of_terms
    try
        let total: int64 = _floordiv64 (int64 ((int64 num_of_terms) * (((int64 2) * (int64 first_term)) + ((int64 (num_of_terms - 1)) * (int64 common_diff))))) (int64 (int64 2))
        __ret <- int total
        raise Return
        __ret
    with
        | Return -> __ret
and test_sum_of_series () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        if (sum_of_series (1) (1) (10)) <> 55 then
            ignore (failwith ("sum_of_series(1, 1, 10) failed"))
        if (sum_of_series (1) (10) (100)) <> 49600 then
            ignore (failwith ("sum_of_series(1, 10, 100) failed"))
        __ret
    with
        | Return -> __ret
and main () =
    let mutable __ret : obj = Unchecked.defaultof<obj>
    try
        let __bench_start = _now()
        let __mem_start = System.GC.GetTotalMemory(true)
        ignore (test_sum_of_series())
        ignore (printfn "%d" (sum_of_series (1) (1) (10)))
        let __bench_end = _now()
        let __mem_end = System.GC.GetTotalMemory(true)
        printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)

        __ret
    with
        | Return -> __ret
ignore (main())
