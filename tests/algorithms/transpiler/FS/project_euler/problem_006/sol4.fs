// Generated 2025-08-23 14:49 +0700

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
let __bench_start = _now()
let __mem_start = System.GC.GetTotalMemory(true)
let rec solution (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let sum_of_squares: int64 = _floordiv64 (int64 ((n * (n + (int64 1))) * (((int64 2) * n) + (int64 1)))) (int64 (int64 6))
        let sum_first_n: int64 = _floordiv64 (int64 (n * (n + (int64 1)))) (int64 (int64 2))
        let square_of_sum: int64 = sum_first_n * sum_first_n
        __ret <- square_of_sum - sum_of_squares
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%A" (solution (int64 10)))
ignore (printfn "%A" (solution (int64 15)))
ignore (printfn "%A" (solution (int64 20)))
ignore (printfn "%A" (solution (int64 50)))
ignore (printfn "%A" (solution (int64 100)))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
