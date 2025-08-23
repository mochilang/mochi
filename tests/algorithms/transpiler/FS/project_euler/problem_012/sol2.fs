// Generated 2025-08-23 15:31 +0700

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
let rec countDivisors (n: int64) =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    let mutable n = n
    try
        let mutable num: int64 = n
        let mutable total: int64 = 1L
        let mutable i: int64 = 2L
        while (i * i) <= num do
            let mutable multiplicity: int64 = 0L
            while (((num % i + i) % i)) = 0L do
                num <- _floordiv64 (int64 num) (int64 i)
                multiplicity <- multiplicity + 1L
            total <- total * (multiplicity + 1L)
            i <- i + 1L
        if num > 1L then
            total <- total * 2L
        __ret <- total
        raise Return
        __ret
    with
        | Return -> __ret
and solution () =
    let mutable __ret : int64 = Unchecked.defaultof<int64>
    try
        let mutable n: int64 = 1L
        let mutable tri: int64 = 1L
        while (countDivisors (tri)) <= 500L do
            n <- n + 1L
            tri <- tri + n
        __ret <- tri
        raise Return
        __ret
    with
        | Return -> __ret
ignore (printfn "%A" (solution()))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
