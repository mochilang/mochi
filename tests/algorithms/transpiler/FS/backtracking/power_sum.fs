// Generated 2025-08-06 20:48 +0700

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
let rec int_pow (``base``: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable ``base`` = ``base``
    let mutable exp = exp
    try
        let mutable result: int = 1
        let mutable i: int = 0
        while i < exp do
            result <- result * ``base``
            i <- i + 1
        __ret <- result
        raise Return
        __ret
    with
        | Return -> __ret
and backtrack (target: int) (exp: int) (current: int) (current_sum: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable target = target
    let mutable exp = exp
    let mutable current = current
    let mutable current_sum = current_sum
    try
        if current_sum = target then
            __ret <- 1
            raise Return
        let p: int = int_pow (current) (exp)
        let mutable count: int = 0
        if (current_sum + p) <= target then
            count <- count + (backtrack (target) (exp) (current + 1) (current_sum + p))
        if p < target then
            count <- count + (backtrack (target) (exp) (current + 1) (current_sum))
        __ret <- count
        raise Return
        __ret
    with
        | Return -> __ret
and solve (target: int) (exp: int) =
    let mutable __ret : int = Unchecked.defaultof<int>
    let mutable target = target
    let mutable exp = exp
    try
        if not ((((1 <= target) && (target <= 1000)) && (2 <= exp)) && (exp <= 10)) then
            printfn "%s" ("Invalid input")
            __ret <- 0
            raise Return
        __ret <- backtrack (target) (exp) (1) (0)
        raise Return
        __ret
    with
        | Return -> __ret
printfn "%d" (solve (13) (2))
printfn "%d" (solve (10) (2))
printfn "%d" (solve (10) (3))
let __bench_end = _now()
let __mem_end = System.GC.GetTotalMemory(true)
printfn "{\n  \"duration_us\": %d,\n  \"memory_bytes\": %d,\n  \"name\": \"main\"\n}" ((__bench_end - __bench_start) / 1000) (__mem_end - __mem_start)
